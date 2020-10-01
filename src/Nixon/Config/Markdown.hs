{-# LANGUAGE OverloadedStrings #-}

module Nixon.Config.Markdown
  ( defaultPath
  , readMarkdown
  , parseMarkdown
  ) where

import           Control.Arrow ((***))
import           Data.Aeson (eitherDecodeStrict)
import           Data.Bifunctor (Bifunctor(first))
import           Data.Maybe (listToMaybe)
import           Data.Text (unpack, pack)
import           Data.Text.Encoding (encodeUtf8)
import           Nixon.Command hiding (parse)
import qualified Nixon.Config.JSON as JSON
import           Prelude hiding (FilePath)
import           System.Directory (XdgDirectory(..), getXdgDirectory)
import           Text.Pandoc (Attr, Block(..))
import qualified Text.Pandoc as P
import qualified Text.Pandoc.Builder as B
import           Text.Pandoc.Walk
import           Turtle hiding (Header, err, filename, text, l, x)


defaultPath :: MonadIO m => m FilePath
defaultPath = liftIO $ fromString <$> getXdgDirectory XdgConfig "nixon.md"


-- | Extract commands from a markdown file
readMarkdown :: MonadIO m => FilePath -> m (JSON.Config, [Command])
readMarkdown filename = liftIO $ P.runIOorExplode $
  liftIO $ parseMarkdown =<< readTextFile filename


-- | Extract commands from a markdown document
parseMarkdown :: MonadIO m => Text -> m (JSON.Config, [Command])
parseMarkdown markdown = liftIO $ P.runIOorExplode $ do
  res <- parse . query extract <$> P.readMarkdown mdOpts markdown
  case res of
    Left errorMsg -> error (unpack errorMsg)
    Right cmds -> pure cmds
  where mdOpts = P.def { P.readerExtensions = P.pandocExtensions }


data Node = Head Int Text Attr -- ^ level name command type
          | Source (Maybe Text) Text -- ^ lang src
          | Paragraph Text
          | NA
          deriving Show


-- | "Tokenize" Pandoc blocks into a list of Nodes
extract :: Block -> [Node]
extract (Header lvl attr@(name, _, _) _) = [Head lvl (pack name) attr]
extract p@(Para _) = [Paragraph $ either (const "") id text]
  where text = P.runPure $ do
          let doc = B.doc (B.singleton p)
          P.writePlain P.def doc
extract (CodeBlock (_, args, _) src) = [Source lang (pack src)]
  where lang = pack <$> listToMaybe args
extract _ = [NA]


data ParseState = S { stateHeaderLevel :: Int
                    , stateProjectTypes :: [Text]
                    }


-- | Parse Command blocks from a list of nodes
parse :: [Node] -> Either Text (JSON.Config, [Command])
parse nodes = go (S 0 []) (JSON.empty, []) nodes
  where
    go _ ps [] = Right ps

    go st ps nodes'@(Head l name _ : _)
      -- Going back up or next sibling
      | l < stateHeaderLevel st = go (S l []) ps nodes'

      -- Skipping levels on the way down
      | l > (stateHeaderLevel st + 1) = Left $ format
        ("Unexpected header level bump ("%d%" to "%d%"): "%s) (stateHeaderLevel st) l name

    go st (cfg, ps) (Head l name attr : rest)
      -- We found a config
      | hasArgs "config" attr = case parseConfig rest of
          (Left err, _) -> Left err
          (Right cfg', rest') -> go st (cfg', ps) rest'

      -- We found a command
      | hasArgs "command" attr =
        let pt = getKwargs "type" attr <> stateProjectTypes st
            isGui = hasArgs "gui" attr
            isJson = hasArgs "json" attr
        in case parseCommand name pt rest of
          (Left err, _) -> Left err
          (Right p, rest') -> go st (cfg, p <! gui isGui <! json isJson : ps) rest'

      -- Pick up project type along the way
      -- TODO: Accumulate multiple types?
      | otherwise = go st' (cfg, ps) rest
        where st' = st { stateHeaderLevel = l
                      , stateProjectTypes = getKwargs "type" attr <> parentTypes
                      }
              parentTypes | l == stateHeaderLevel st = []
                          | otherwise = stateProjectTypes st

    -- All other nodes are ignored
    go st ps (_ : rest) = go st ps rest


hasArgs :: Text -> Attr -> Bool
hasArgs key (_, args, _) = unpack key `elem` args


getKwargs :: Text -> Attr -> [Text]
getKwargs key (_, _, kwargs) = map snd $ filter ((== key) . fst) $ map (pack *** pack) kwargs


parseConfig :: [Node] -> (Either Text JSON.Config, [Node])
parseConfig (Source lang src : rest') = case lang of
  Nothing -> (parsed, rest')
  Just "json" -> (parsed, rest')
  Just lang' -> (Left $ format ("Invalid config language: "%s) lang', rest')
  where parsed :: Either Text JSON.Config
        parsed = first pack (eitherDecodeStrict $ encodeUtf8 src)
parseConfig rest = (Left "Expecting config source after header", rest)


parseCommand :: Text -> [Text] -> [Node] -> (Either Text Command, [Node])
parseCommand name projectTypes (Paragraph desc : rest) =
  let (cmd, rest') = parseCommand name projectTypes rest
  in  (description desc <$> cmd, rest')
parseCommand name projectTypes (Source (Just lang) src : rest) = (cmd, rest)
  where cmd = mkcommand name lang projectTypes src
parseCommand name _ rest = (Left $ format ("Expecting source block for "%s) name, rest)
