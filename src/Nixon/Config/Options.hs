{-# LANGUAGE CPP #-}

module Nixon.Config.Options
  ( Completer,
    CompletionType (..),
    BackendType (..),
    LogLevel (..),
    Options (..),
    SubCommand (..),
    EditOpts (..),
    EvalOpts (..),
    EvalSource (..),
    GCOpts (..),
    NewOpts (..),
    ProjectOpts (..),
    RunOpts (..),
    parseArgs,
  )
where

import Data.Maybe (fromMaybe)
import qualified Data.Text as Text
import Nixon.Command.Placeholder (Placeholder)
import Nixon.Config (readConfig)
import qualified Nixon.Config.Markdown as MD
import Nixon.Config.Types (BackendType (..), Config, ConfigError (..), LogLevel (..))
import qualified Nixon.Config.Types as Config
import qualified Nixon.Language as Lang
import Nixon.Prelude
import Nixon.Utils (implode_home)
import qualified Options.Applicative as Opts
import System.Environment (getArgs)
import Turtle
  ( Alternative (many),
#if MIN_VERSION_GLASGOW_HASKELL(9, 6, 0, 0)
#else
    Applicative (liftA2),
#endif
    IsString (fromString),
    Parser,
    format,
    fp,
    opt,
    optPath,
    optText,
    optional,
    options,
    subcommand,
    switch,
    (%),
  )

type Completer = [String] -> IO [String]

data CompletionType = Edit | Eval | Project | Run

-- | Command line options.
data Options = Options
  { configFile :: Maybe FilePath,
    config :: Config,
    subCommand :: SubCommand
  }
  deriving (Show)

data SubCommand
  = EditCommand EditOpts
  | EvalCommand EvalOpts
  | GCCommand GCOpts
  | NewCommand NewOpts
  | ProjectCommand ProjectOpts
  | RunCommand RunOpts
  deriving (Show)

newtype EditOpts = EditOpts {editCommand :: Maybe Text}
  deriving (Show)

data EvalSource = EvalInline Text | EvalFile FilePath
  deriving (Show)

data EvalOpts = EvalOpts
  { evalSource :: EvalSource,
    evalPlaceholders :: [Placeholder],
    evalLanguage :: Maybe Lang.Language,
    evalSelectProject :: Bool
  }
  deriving (Show)

newtype GCOpts = GCOpts
  { gcDryRun :: Bool
  }
  deriving (Show)

data NewOpts = NewOpts
  { name :: Text,
    description :: Text,
    language :: Lang.Language,
    source :: Text
  }
  deriving (Show)

data ProjectOpts = ProjectOpts
  { projProject :: Maybe Text,
    projCommand :: Maybe Text,
    projArgs :: [Text],
    projInsert :: Bool,
    projInspect :: Bool,
    projList :: Bool,
    projSelect :: Bool
  }
  deriving (Show)

data RunOpts = RunOpts
  { runCommand :: Maybe Text,
    runArgs :: [Text],
    runInsert :: Bool,
    runList :: Bool,
    runSelect :: Bool
  }
  deriving (Show)

-- | Add options supporting negation with a "no-" prefix.
maybeSwitch :: Text -> Char -> Text -> Parser (Maybe Bool)
maybeSwitch long short help =
  Opts.flag
    Nothing
    (Just True)
    ( Opts.short short
        <> Opts.long (Text.unpack long)
        <> Opts.help (Text.unpack help)
    )
    <|> Opts.flag
      Nothing
      (Just False)
      ( Opts.long ("no-" ++ Text.unpack long)
      )

parser :: FilePath -> (CompletionType -> Opts.Completer) -> Parser Options
parser default_config mkcompleter =
  Options
    <$> optional (optPath "config" 'C' (configHelp default_config))
    <*> parseConfig
    <*> ( EditCommand
            <$> subcommand "edit" "Edit a command in $EDITOR" (editParser $ mkcompleter Edit)
            <|> EvalCommand
            <$> subcommand "eval" "Evaluate expression" evalParser
            <|> GCCommand
            <$> subcommand "gc" "Garbage collect cached items" gcParser
            <|> NewCommand
            <$> subcommand "new" "Create a new command" newParser
            <|> ProjectCommand
            <$> subcommand "project" "Project actions" (projectParser mkcompleter)
            <|> RunCommand
            <$> subcommand "run" "Run command" (runParser $ mkcompleter Run)
            <|> RunCommand
            <$> runParser (mkcompleter Run)
        )
  where
    parseBackend =
      flip
        lookup
        [ ("fzf", Fzf),
          ("rofi", Rofi)
        ]
    parseLoglevel =
      flip
        lookup
        [ ("debug", LogDebug),
          ("info", LogInfo),
          ("warning", LogWarning),
          ("warn", LogWarning),
          ("error", LogError)
        ]
    configHelp = fromString . Text.unpack . format ("Path to configuration file (default: " % fp % ")")
    parseConfig =
      Config.Config
        <$> optional (opt parseBackend "backend" 'b' "Backend to use: fzf, rofi")
        <*> pure [] -- Bin dirs are not CLI args
        <*> maybeSwitch "exact" 'e' "Enable exact match"
        <*> maybeSwitch "ignore-case" 'i' "Case-insensitive match"
        <*> maybeSwitch "force-tty" 'T' "Never fork or spawn off separate processes"
        <*> many (optPath "path" 'p' "Project directory")
        <*> pure [] -- Project types are not CLI args
        <*> pure [] -- Commands are not CLI args
        <*> maybeSwitch "direnv" 'd' "Evaluate .envrc files using `direnv exec`"
        <*> maybeSwitch "nix" 'n' "Invoke nix-shell if *.nix files are found"
        <*> optional (optText "terminal" 't' "Terminal emultor for non-GUI commands")
        <*> optional (opt parseLoglevel "loglevel" 'L' "Loglevel: debug, info, warning, error")

editParser :: Opts.Completer -> Parser EditOpts
editParser completer =
  EditOpts
    <$> optional (Opts.strArgument $ Opts.metavar "command" <> Opts.help "Command to edit" <> Opts.completer completer)

evalParser :: Parser EvalOpts
evalParser =
  EvalOpts
    <$> ( EvalFile
            <$> optPath "file" 'f' "File to evaluate"
            <|> EvalInline
            <$> Opts.strArgument
              (Opts.metavar "command" <> Opts.help "Command expression")
        )
    <*> many
      ( Opts.argument
          (Opts.eitherReader MD.parseCommandArg)
          (Opts.metavar "placeholder" <> Opts.help "Placeholder")
      )
    <*> optional (opt parseLang "language" 'l' "Language: bash, JavaScript, Haskell, ...")
    <*> switch "project" 'p' "Select project"

gcParser :: Parser GCOpts
gcParser =
  GCOpts <$> switch "dry-run" 'd' "Dry-run, print file paths without deleting"

newParser :: Parser NewOpts
newParser =
  ( NewOpts
      . fromMaybe "<name>"
      <$> optional (optText "name" 'n' "Command name")
  )
    <*> (fromMaybe "Description…" <$> optional (optText "desc" 'd' "Command description"))
    <*> (fromMaybe Lang.Bash <$> optional (opt parseLang "lang" 'l' "Language: bash, JavaScript, Haskell, ..."))
    <*> (fromMaybe "" <$> optional (optText "src" 's' "Command source code"))

parseLang :: Text -> Maybe Lang.Language
parseLang = Just . Lang.parseLang

projectParser :: (CompletionType -> Opts.Completer) -> Parser ProjectOpts
projectParser mkcompleter =
  ProjectOpts
    <$> optional
      ( Opts.strArgument
          $ Opts.metavar "project"
          <> Opts.help "Project to jump into"
          <> Opts.completer (mkcompleter Project)
      )
    <*> optional
      ( Opts.strArgument
          $ Opts.metavar "command"
          <> Opts.help "Command to run"
          <> Opts.completer (mkcompleter Run)
      )
    <*> many (Opts.strArgument $ Opts.metavar "args..." <> Opts.help "Arguments to command")
    <*> switch "insert" 'i' "Select a project command and output its source"
    <*> switch "inspect" 'I' "Select a project and list some info about it"
    <*> switch "list" 'l' "List projects"
    <*> switch "select" 's' "Select a project and output on stdout"

runParser :: Opts.Completer -> Parser RunOpts
runParser completer =
  RunOpts
    <$> optional (Opts.strArgument $ Opts.metavar "command" <> Opts.help "Command to run" <> Opts.completer completer)
    <*> many (Opts.strArgument $ Opts.metavar "args..." <> Opts.help "Arguments to command")
    <*> switch "insert" 'i' "Select a command and output its source"
    <*> switch "list" 'l' "List commands"
    <*> switch "select" 's' "Output command selection on stdout"

-- | Read configuration from config file and command line arguments
parseArgs :: (MonadIO m) => (CompletionType -> Completer) -> m (Either ConfigError (SubCommand, Config))
parseArgs mkcompleter = do
  defaultConfig <- implode_home =<< MD.defaultPath
  let completer = Opts.listIOCompleter . completionArgs . mkcompleter
  opts <- Turtle.options "Command & environment launcher" (parser defaultConfig completer)
  configPath <- maybe MD.defaultPath pure (configFile opts)
  liftIO $ do
    cfg <- readConfig configPath
    let mergedConfig = liftA2 (<>) cfg (pure $ config opts)
        subCmdConfig = fmap (subCommand opts,) mergedConfig
    pure subCmdConfig

completionArgs :: Completer -> IO [String]
completionArgs completer = completer . drop 1 . extractWords =<< liftIO getArgs
  where
    extractWords ("--bash-completion-word" : w' : ws) = w' : extractWords ws
    extractWords (_ : ws) = extractWords ws
    extractWords [] = []
