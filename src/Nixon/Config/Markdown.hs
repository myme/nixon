{-# LANGUAGE OverloadedStrings #-}

module Nixon.Config.Markdown
  ( defaultPath,
    parseMarkdown,
    parseHeaderArgs,
    parseCommandArg,
    parseCommandName,
  )
where

import CMark (commonmarkToNode)
import qualified CMark as M
import Data.Aeson (eitherDecodeStrict)
import Data.Bifunctor (Bifunctor (first))
import Data.Char (isSpace)
import Data.Either (partitionEithers)
import Data.List (find)
import Data.List.NonEmpty (NonEmpty ((:|)))
import Data.Text (pack, strip)
import qualified Data.Text as T
import Data.Text.Encoding (encodeUtf8)
import Nixon.Command (bg, json, (<!))
import qualified Nixon.Command as Cmd
import qualified Nixon.Command.Placeholder as Cmd
import qualified Nixon.Config.JSON as JSON
import Nixon.Config.Types
  ( Config
      ( bin_dirs,
        commands,
        exact_match,
        ignore_case,
        project_dirs,
        project_types,
        use_direnv,
        use_nix
      ),
    defaultConfig,
  )
import qualified Nixon.Language as Lang
import Nixon.Prelude
import System.Directory (XdgDirectory (..), getXdgDirectory)
import qualified Text.Parsec as P
import Text.Parsec.Text (Parser)
import Turtle
  ( IsString (fromString),
    d,
    format,
    s,
    (%),
  )

data PosInfo = PosInfo
  { posName :: FilePath,
    posLocation :: Maybe M.PosInfo
  }

defaultPath :: MonadIO m => m FilePath
defaultPath = liftIO $ fromString <$> getXdgDirectory XdgConfig "nixon.md"

buildConfig :: (JSON.Config, [Cmd.Command]) -> Config
buildConfig (cfg, cmds) =
  defaultConfig
    { bin_dirs = JSON.bin_dirs cfg,
      exact_match = JSON.exact_match cfg,
      ignore_case = JSON.ignore_case cfg,
      project_dirs = JSON.project_dirs cfg,
      project_types = JSON.project_types cfg,
      use_direnv = JSON.use_direnv cfg,
      use_nix = JSON.use_nix cfg,
      commands = cmds
    }

parseMarkdown :: FilePath -> Text -> Either Text Config
parseMarkdown fileName markdown =
  buildConfig
    <$> parse fileName (extract (commonmarkToNode [] markdown))

extract :: M.Node -> [Node]
extract (M.Node pos nodeType children) = case nodeType of
  M.HEADING level ->
    let (name, args, kwargs) = parseHeaderArgs $ getText children
        (isCommand, isBg) = case find isCode children of
          Just (M.Node _ (M.CODE text) _) -> (True, "&" `T.isSuffixOf` T.strip text)
          _ -> (False, False)
        args' =
          ["bg" | isBg && "bg" `notElem` args]
            ++ ["command" | isCommand && "command" `notElem` args]
            ++ args
     in [Head pos level name (name, args', kwargs)]
  M.CODE_BLOCK info text ->
    let lang = case T.words info of
          [] -> Nothing
          (l : _) -> Just l
     in [Source lang text]
  M.PARAGRAPH -> [Paragraph $ getText children]
  _ -> concatMap extract children
  where
    isCode (M.Node _ (M.CODE _) _) = True
    isCode _ = False

type Attrs =
  -- | name args kwargs
  (Text, [Text], [(Text, Text)])

parseHeaderArgs :: Text -> Attrs
parseHeaderArgs input = case P.parse parser "" input of
  Left _ -> (input, [], [])
  Right xs -> xs
  where
    parser :: Parser Attrs
    parser = do
      name <- T.strip . T.pack <$> P.many (P.noneOf ['{'])
      (args, kwargs) <- partitionEithers <$> braces (P.sepBy (parseArg <|> parseKwArg) P.spaces)
      pure (name, args, kwargs)
    parseArg = do
      name <- T.pack <$> (P.char '.' *> identifier)
      pure $ Left name
    parseKwArg = do
      name <- T.pack <$> identifier
      value <- T.pack <$> (P.char '=' *> (quotes letters <|> letters))
      pure $ Right (name, value)
    letters = P.many1 P.letter
    identifier = P.many1 (P.letter <|> P.char '_' <|> P.char '-')
    braces = P.between (P.char '{') (P.char '}')
    quotes = P.between (P.char '"') (P.char '"')

getText :: [M.Node] -> Text
getText [] = ""
getText (M.Node _ nodeType children : xs) = T.strip $ T.intercalate " " [nodeText, getText children, getText xs]
  where
    nodeText = case nodeType of
      M.TEXT txt -> txt
      M.HTML_BLOCK txt -> txt
      M.HTML_INLINE txt -> txt
      M.CODE txt -> txt
      M.CODE_BLOCK _ txt -> txt
      _ -> ""

type Pos = Maybe M.PosInfo

data Node
  = -- | level name command type
    Head Pos Int Text Attrs
  | -- | lang src
    Source (Maybe Text) Text
  | Paragraph Text
  deriving (Show)

data ParseState = S
  { stateHeaderLevel :: Int,
    stateProjectTypes :: [Text]
  }

-- | Parse Command blocks from a list of nodes
parse :: FilePath -> [Node] -> Either Text (JSON.Config, [Cmd.Command])
parse fileName = go (S 0 []) (JSON.empty, [])
  where
    go _ ps [] = Right ps
    go st ps nodes'@(Head _ l name _ : _)
      -- Going back up or next sibling
      | l < stateHeaderLevel st = go (S l []) ps nodes'
      -- Skipping levels on the way down
      | l > stateHeaderLevel st + 1 =
          Left $
            format
              ("Unexpected header level bump (" % d % " to " % d % "): " % s)
              (stateHeaderLevel st)
              l
              name
    go st (cfg, ps) (Head pos l name attr : rest)
      -- We found a config
      | hasArgs "config" attr = case parseConfig rest of
          (Left err, _) -> Left err
          (Right cfg', rest') -> go st (cfg', ps) rest'
      -- We found a command
      | hasArgs "command" attr =
          let pt = getKwargs "type" attr <> stateProjectTypes st
              isBg = hasArgs "bg" attr
              isJson = hasArgs "json" attr
           in case parseCommand (PosInfo fileName pos) name pt rest of
                (Left err, _) -> Left err
                (Right p, rest') -> go st (cfg, p <! bg isBg <! json isJson : ps) rest'
      -- Pick up project type along the way
      | otherwise = go st' (cfg, ps) rest
      where
        st' =
          st
            { stateHeaderLevel = l,
              stateProjectTypes = getKwargs "type" attr <> parentTypes
            }
        parentTypes
          | l == stateHeaderLevel st = []
          | otherwise = stateProjectTypes st

    -- All other nodes are ignored
    go st ps (_ : rest) = go st ps rest

hasArgs :: Text -> Attrs -> Bool
hasArgs key (_, args, _) = key `elem` args

getKwargs :: Text -> Attrs -> [Text]
getKwargs key (_, _, kwargs) = map snd $ filter ((== key) . fst) kwargs

parseConfig :: [Node] -> (Either Text JSON.Config, [Node])
parseConfig (Source lang src : rest') = case lang of
  Nothing -> (parsed, rest')
  Just "json" -> (parsed, rest')
  Just lang' -> (Left $ format ("Invalid config language: " % s) lang', rest')
  where
    parsed :: Either Text JSON.Config
    parsed = first pack (eitherDecodeStrict $ encodeUtf8 src)
parseConfig rest = (Left "Expecting config source after header", rest)

parseCommand :: PosInfo -> Text -> [Text] -> [Node] -> (Either Text Cmd.Command, [Node])
parseCommand pos name projectTypes (Paragraph desc : rest) =
  let (cmd, rest') = parseCommand pos name projectTypes rest
   in (Cmd.description (strip desc) <$> cmd, rest')
parseCommand pos name projectTypes (Source lang src : rest) = (cmd, rest)
  where
    loc =
      Cmd.CommandLocation
        { Cmd.cmdFilePath = posName pos,
          Cmd.cmdLineNr = maybe (-1) M.startLine (posLocation pos)
        }
    cmd = do
      (name', args) <- parseCommandName name
      pure
        Cmd.empty
          { Cmd.cmdName = name',
            Cmd.cmdLang = maybe Lang.None Lang.parseLang lang,
            Cmd.cmdPlaceholders = args,
            Cmd.cmdProjectTypes = projectTypes,
            Cmd.cmdSource = src,
            Cmd.cmdLocation = Just loc,
            Cmd.cmdIsHidden = "_" `T.isPrefixOf` name'
          }
parseCommand _ name _ rest = (Left $ format ("Expecting source block for " % s) name, rest)

parseCommandName :: Text -> Either Text (Text, [Cmd.Placeholder])
parseCommandName = first (T.pack . show) . P.parse parser ""
  where
    parser = do
      P.spaces
      name <- T.pack <$> P.many1 (P.satisfy (not . isSpace))
      args <- parseCommandArgs
      pure (name, args)

parseCommandArgs :: Parser [Cmd.Placeholder]
parseCommandArgs =
  P.choice
    [ (:) <$> parseCommandArg' <*> parseCommandArgs,
      P.anyChar *> parseCommandArgs,
      [] <$ P.eof
    ]

-- | Convenience wrapper for running placeholder parser
parseCommandArg :: String -> Either String Cmd.Placeholder
parseCommandArg = first show . P.parse parseCommandArg' "" . T.pack

parseCommandArg' :: Parser Cmd.Placeholder
parseCommandArg' = do
  let startCmdArg =
        (Cmd.Stdin <$ P.char '<')
          <|> (Cmd.Arg <$ P.char '$')
          <|> (Cmd.EnvVar . T.pack <$> P.many P.alphaNum <* P.char '=')
  placeholderType <- P.try $ startCmdArg <* P.char '{'
  spec <- T.pack <$> P.manyTill (P.noneOf "}") (P.char '}')
  let (name :| flags) = splitOn ":" spec
      multiple = "m" `elem` flags
      fixup = T.replace "-" "_"
      placeholderWithName = case placeholderType of
        Cmd.EnvVar "" -> Cmd.EnvVar $ fixup name
        Cmd.EnvVar alias -> Cmd.EnvVar $ fixup alias
        same -> same
  pure $ Cmd.Placeholder placeholderWithName name multiple

splitOn :: Text -> Text -> NonEmpty Text
splitOn delim input = case T.splitOn delim input of
  [] -> input :| []
  (first' : rest) -> first' :| rest
