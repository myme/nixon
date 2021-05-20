{-# LANGUAGE OverloadedStrings #-}

module Nixon.Config.Markdown
  ( defaultPath
  , parseMarkdown
  , parseMarkdown'
  , parseHeaderArgs
  ) where

import           CMark (commonmarkToNode)
import qualified CMark as M
import           Data.Aeson (eitherDecodeStrict)
import           Data.Bifunctor (Bifunctor(first))
import           Data.Either (fromRight, partitionEithers)
import           Data.List (find)
import           Data.Maybe (listToMaybe)
import           Data.Text (isSuffixOf, pack, strip)
import qualified Data.Text as T
import           Data.Text.Encoding (encodeUtf8)
import           Nixon.Command ((<!), bg, json)
import qualified Nixon.Command as Cmd
import qualified Nixon.Config.JSON as JSON
import           Nixon.Config.Types
import           Prelude hiding (FilePath)
import           System.Directory (XdgDirectory(..), getXdgDirectory)
import qualified Text.Pandoc as P
import qualified Text.Pandoc.Builder as B
import           Text.Pandoc.Walk
import qualified Text.Parsec as P
import           Text.Parsec.Text (Parser)
import           Turtle hiding (Header, Parser, err, filename, find, input, text, l, x)


defaultPath :: MonadIO m => m FilePath
defaultPath = liftIO $ fromString <$> getXdgDirectory XdgConfig "nixon.md"


-- | Extract commands from a markdown document
parseMarkdown :: Text -> Either Text Config
parseMarkdown markdown = do
  nodes <- first (pack . show) . P.runPure $ query extract <$> P.readMarkdown mdOpts markdown
  buildConfig <$> parse nodes
  where mdOpts = P.def { P.readerExtensions = P.pandocExtensions }


buildConfig :: (JSON.Config, [Cmd.Command]) -> Config
buildConfig (cfg, cmds) = defaultConfig
  { exact_match = JSON.exact_match cfg
  , ignore_case = JSON.ignore_case cfg
  , project_dirs = JSON.project_dirs cfg
  , project_types = JSON.project_types cfg
  , use_direnv = JSON.use_direnv cfg
  , use_nix = JSON.use_nix cfg
  , commands = cmds
  }


parseMarkdown' :: Text -> Either Text Config
parseMarkdown' markdown = buildConfig <$> parse (extract' (commonmarkToNode [] markdown))


extract' :: M.Node -> [Node]
extract' (M.Node _ nodeType children) = case nodeType of
  M.HEADING level -> let name = getText children
                         (args, kwargs) = parseHeaderArgs name
                         (isCommand, isBg) = case find isCode children of
                           Just (M.Node _ (M.CODE text) _) -> (True, "&" `T.isSuffixOf` T.strip text)
                           _ -> (False, False)
                         args' = ["bg" | isBg && "bg" `notElem` args] ++
                                 ["command" | isCommand && "command" `notElem` args] ++
                                 args
                     in [Head level name (name, args', kwargs)]
  M.CODE_BLOCK info text -> let lang = case T.words info of
                                  [] -> Nothing
                                  (l:_) -> Just l
                            in [Source lang text]
  M.PARAGRAPH -> [Paragraph $ getText children]
  _ -> concatMap extract' children
  where isCode (M.Node _ (M.CODE _) _) = True
        isCode _ =  False


parseHeaderArgs :: Text -> ([Text], [(Text, Text)])
parseHeaderArgs input = case P.parse parser "" input of
  Left _   -> ([], [])
  Right xs -> xs
  where parser :: Parser ([Text], [(Text, Text)])
        parser = do
          P.skipMany $ P.noneOf ['{']
          args <- braces (P.sepBy (parseArg <|> parseKwArg) P.spaces)
          pure $ partitionEithers args
        parseArg = do
          name <- T.pack <$> (P.char '.' *> letters)
          pure $ Left name
        parseKwArg = do
          name  <- T.pack <$> P.many1 P.letter
          value <- T.pack <$> (P.char '=' *> (quotes letters <|> letters))
          pure $ Right (name, value)
        letters = P.many1 P.letter
        braces = P.between (P.char '{') (P.char '}')
        quotes = P.between (P.char '"') (P.char '"')


getText :: [M.Node] -> Text
getText [] = ""
getText (M.Node _ nodeType children : xs) = T.strip $ T.intercalate " " [nodeText, getText children, getText xs]
  where nodeText = case nodeType of
          M.TEXT txt -> txt
          M.HTML_BLOCK txt -> txt
          M.HTML_INLINE txt -> txt
          M.CODE txt -> txt
          M.CODE_BLOCK _ txt -> txt
          _ -> ""


data Node = Head Int Text P.Attr -- ^ level name command type
          | Source (Maybe Text) Text -- ^ lang src
          | Paragraph Text
          deriving Show


-- | "Tokenize" Pandoc blocks into a list of Nodes
extract :: P.Block -> [Node]
extract (P.Header lvl (name, args, kwargs) children) =
  let (name', args') = case find isCommand children of
        Just (P.Code _ txt) -> (txt,
          ["command" | "command" `notElem` args] ++
          ["bg" | "bg" `notElem` args && "&" `isSuffixOf` txt] ++
          args)
        _ -> (name, args)
  in [Head lvl name' (name, args', kwargs)]
  where isCommand (P.Code _ _) = True
        isCommand _            = False
extract p@(P.Para _) = [Paragraph $ fromRight "" text]
  where text = P.runPure $ do
          let doc = B.doc (B.singleton p)
          P.writePlain P.def doc
extract (P.CodeBlock (_, args, _) src) = [Source (listToMaybe args) src]
extract _ = []


data ParseState = S { stateHeaderLevel :: Int
                    , stateProjectTypes :: [Text]
                    }


-- | Parse Command blocks from a list of nodes
parse :: [Node] -> Either Text (JSON.Config, [Cmd.Command])
parse = go (S 0 []) (JSON.empty, [])
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
            isBg = hasArgs "bg" attr
            isJson = hasArgs "json" attr
        in case parseCommand name pt rest of
          (Left err, _) -> Left err
          (Right p, rest') -> go st (cfg, p <! bg isBg <! json isJson : ps) rest'

      -- Pick up project type along the way
      | otherwise = go st' (cfg, ps) rest
        where st' = st { stateHeaderLevel = l
                       , stateProjectTypes = getKwargs "type" attr <> parentTypes
                       }
              parentTypes | l == stateHeaderLevel st = []
                          | otherwise = stateProjectTypes st

    -- All other nodes are ignored
    go st ps (_ : rest) = go st ps rest


hasArgs :: Text -> P.Attr -> Bool
hasArgs key (_, args, _) = key `elem` args


getKwargs :: Text -> P.Attr -> [Text]
getKwargs key (_, _, kwargs) = map snd $ filter ((== key) . fst) kwargs


parseConfig :: [Node] -> (Either Text JSON.Config, [Node])
parseConfig (Source lang src : rest') = case lang of
  Nothing -> (parsed, rest')
  Just "json" -> (parsed, rest')
  Just lang' -> (Left $ format ("Invalid config language: "%s) lang', rest')
  where parsed :: Either Text JSON.Config
        parsed = first pack (eitherDecodeStrict $ encodeUtf8 src)
parseConfig rest = (Left "Expecting config source after header", rest)


parseCommand :: Text -> [Text] -> [Node] -> (Either Text Cmd.Command, [Node])
parseCommand name projectTypes (Paragraph desc : rest) =
  let (cmd, rest') = parseCommand name projectTypes rest
  in  (Cmd.description (strip desc) <$> cmd, rest')
parseCommand name projectTypes (Source lang src : rest) = (cmd, rest)
  where cmd = Cmd.mkcommand name lang projectTypes src
parseCommand name _ rest = (Left $ format ("Expecting source block for "%s) name, rest)
