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
import           Turtle hiding (err, select)

-- TODO: Add CLI opt for outputting bash/zsh completion script.
--       https://github.com/pcapriotti/optparse-applicative#bash-zsh-and-fish-completions
-- TODO: Add support for independent directory/tree of nix files.
--       The idea is that for some projects you don't want to "pollute" the
--       project by adding e.g. nix files. Add support so that "nixon" can find
--       these files and launch the appropriate environment without the files
--       having to be *in* the project root.
-- TODO: Add "Backend" configuration support (for e.g. styles like height)
-- E.g. --backend-arg "fzf: --height 40"
--      --backend-arg "rofi: ..."
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

-- TODO: Allow switching off "use_direnv" and "use_nix"
parser :: FilePath -> Parser Options
parser default_config = Options
  <$> optional (optPath "config" 'C' (config_help default_config))
  <*> parse_config
  <*> ( ProjectCommand <$> subcommand "project" "Project actions" project_parser <|>
        RunCommand <$> subcommand "run" "Run command" run_parser <|>
        RunCommand <$> run_parser)
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
      <*> many (optPath "path" 'p' "Project directory")
      <*> pure [] -- Project types are not CLI args
      <*> pure [] -- Commands are not CLI args
      <*> maybeSwitch "direnv" 'd' "Evaluate .envrc files using `direnv exec`"
      <*> maybeSwitch "nix" 'n' "Invoke nix-shell if *.nix files are found"
      <*> optional (optText "terminal" 't' "Terminal emultor for non-GUI commands")
      <*> optional (opt parse_loglevel "loglevel" 'l' "Loglevel: debug, info, warning, error")

project_parser :: Parser ProjectOpts
project_parser = ProjectOpts
  <$> optional (argText "project" "Project to jump into")
  <*> optional (argText "command" "Command to run")
  <*> switch "list" 'l' "List projects"
  <*> switch "select" 's' "Select a project or command and output on stdout"

run_parser :: Parser RunOpts
run_parser = RunOpts
  <$> optional (argText "command" "Command to run")
  <*> switch "list" 'l' "List projects"

-- | Read configuration from config file and command line arguments
parse_args :: MonadIO m => m (Either ConfigError (SubCommand, Config))
parse_args = do
  default_config <- implode_home =<< MD.defaultPath
  opts <- Turtle.options "Launch project environment" (parser default_config)
  config_path <- maybe MD.defaultPath pure (config_file opts)
  liftIO $ do
    cfg <- read_config config_path
    let mergedConfig = liftA2 (<>) cfg (pure $ config opts)
        subCmdConfig = fmap (sub_command opts,) mergedConfig
    pure subCmdConfig
