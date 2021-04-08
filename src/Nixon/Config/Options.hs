module Nixon.Config.Options
  ( Backend(..)
  , LogLevel(..)
  , Options(..)
  , SubCommand(..)
  , ProjectOpts(..)
  , RunOpts(..)
  , command
  , default_options
  , parse_args
  ) where

import qualified Data.Text as Text
import           Nixon.Config (read_config)
import qualified Nixon.Config.Markdown as MD
import           Nixon.Config.Types (Backend(..), Config, ConfigError(..), LogLevel(..))
import qualified Nixon.Config.Types as Config
import           Nixon.Utils (implode_home)
import qualified Options.Applicative as Opts
import           Prelude hiding (FilePath)
import           System.Environment (getArgs)
import           Turtle hiding (err, select)

-- | Command line options.
data Options = Options
  { config_file :: Maybe FilePath
  , config :: Config
  , sub_command :: SubCommand
  } deriving Show

data SubCommand = ProjectCommand ProjectOpts
                | RunCommand RunOpts
                deriving Show

data ProjectOpts = ProjectOpts
  { proj_project :: Maybe Text
  , proj_command :: Maybe Text
  , proj_list :: Bool
  , proj_select :: Bool
  } deriving Show

data RunOpts = RunOpts
  { run_command :: Maybe Text
  , run_list :: Bool
  , run_select :: Bool
  } deriving Show

command :: SubCommand -> Maybe Text
command (ProjectCommand ProjectOpts { proj_command }) = proj_command
command (RunCommand RunOpts { run_command }) = run_command

default_options :: Options
default_options = Options
  { config_file = Nothing
  , config = Config.defaultConfig
  , sub_command = RunCommand RunOpts
    { run_command = Nothing
    , run_list = False
    , run_select = False
    }
  }

-- | Add options supporting negation with a "no-" prefix.
maybeSwitch :: Text -> Char -> Text -> Parser (Maybe Bool)
maybeSwitch long short help =
  Opts.flag Nothing (Just True) (
    Opts.short short <>
    Opts.long (Text.unpack long) <>
    Opts.help (Text.unpack help)) <|>
  Opts.flag Nothing (Just False) (
    Opts.long ("no-" ++ Text.unpack long))

parser :: FilePath -> Opts.Completer -> Parser Options
parser default_config completer = Options
  <$> optional (optPath "config" 'C' (config_help default_config))
  <*> parse_config
  <*> ( ProjectCommand <$> subcommand "project" "Project actions" (project_parser completer) <|>
        RunCommand <$> subcommand "run" "Run command" (run_parser completer) <|>
        RunCommand <$> run_parser completer)
  where
    parse_backend = flip lookup
      [("fzf", Fzf)
      ,("rofi", Rofi)
      ]
    parse_loglevel = flip lookup
      [("debug", LogDebug)
      ,("info", LogInfo)
      ,("warning", LogWarning)
      ,("warn", LogWarning)
      ,("error", LogError)
      ]
    config_help = fromString . Text.unpack . format ("Path to configuration file (default: "%fp%")")
    parse_config = Config.Config
      <$> optional (opt parse_backend "backend" 'b' "Backend to use: fzf, rofi")
      <*> maybeSwitch "exact" 'e' "Enable exact match"
      <*> maybeSwitch "ignore-case" 'i' "Case-insensitive match"
      <*> maybeSwitch "force-tty" 'T' "Never fork or spawn off separate processes"
      <*> many (optPath "path" 'p' "Project directory")
      <*> pure [] -- Project types are not CLI args
      <*> pure [] -- Commands are not CLI args
      <*> maybeSwitch "direnv" 'd' "Evaluate .envrc files using `direnv exec`"
      <*> maybeSwitch "nix" 'n' "Invoke nix-shell if *.nix files are found"
      <*> optional (optText "terminal" 't' "Terminal emultor for non-GUI commands")
      <*> optional (opt parse_loglevel "loglevel" 'l' "Loglevel: debug, info, warning, error")

project_parser :: Opts.Completer -> Parser ProjectOpts
project_parser completer = ProjectOpts
  <$> optional (Opts.strArgument $ Opts.metavar "project" <> Opts.help "Project to jump into" <> Opts.completer completer)
  <*> optional (argText "command" "Command to run")
  <*> switch "list" 'l' "List projects"
  <*> switch "select" 's' "Select a project and output on stdout"

run_parser :: Opts.Completer -> Parser RunOpts
run_parser completer = RunOpts
  <$> optional (Opts.strArgument $ Opts.metavar "command" <> Opts.help "Command to run" <> Opts.completer completer)
  <*> switch "list" 'l' "List commands"
  <*> switch "select" 's' "Select a command and output on stdout"

-- | Read configuration from config file and command line arguments
parse_args :: MonadIO m => ([String] -> IO [String]) -> m (Either ConfigError (SubCommand, Config))
parse_args completer = do
  default_config <- implode_home =<< MD.defaultPath
  let completer' = Opts.listIOCompleter $ completion_args completer
  opts <- Turtle.options "Launch project environment" (parser default_config completer')
  config_path <- maybe MD.defaultPath pure (config_file opts)
  liftIO $ do
    cfg <- read_config config_path
    let mergedConfig = liftA2 (<>) cfg (pure $ config opts)
        subCmdConfig = fmap (sub_command opts,) mergedConfig
    pure subCmdConfig

completion_args :: ([String] -> IO [String]) -> IO [String]
completion_args completer = completer . drop 1 . extract_words =<< getArgs
  where extract_words ("--bash-completion-word" : w' : ws) = w' : extract_words ws
        extract_words (_ : ws) = extract_words ws
        extract_words [] = []
