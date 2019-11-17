module Envix.Config.Options
  ( Backend(..)
  , Options(..)
  , SubCommand(..)
  , BuildOpts(..)
  , ProjectOpts(..)
  , default_options
  , merge_opts
  , parse_args
  ) where

import           Control.Monad.Trans.Except
import           Data.Maybe (fromMaybe)
import           Data.Text (Text)
import           Envix.Config.JSON (JSONError(..))
import qualified Envix.Config.JSON as JSON
import           Prelude hiding (FilePath)
import           Turtle hiding (select)

-- TODO: Add CLI opt for outputting bash/zsh completion script.
-- TODO: Add support for independent directory/tree of nix files.
--       The idea is that for some projects you don't want to "pollute" the
--       project by adding e.g. nix files. Add support so that "envix" can find
--       these files and launch the appropriate environment without the files
--       having to be *in* the project root.
-- TODO: Add "Backend" configuration support (for e.g. styles like height)
-- E.g. --backend-arg "fzf: --height 40"
--      --backend-arg "rofi: ..."
-- | Command line options.
data Options = Options
  { backend :: Maybe Backend
    -- , backend_args :: [Text]
  , source_dirs :: [FilePath]
  , use_direnv :: Bool
  , use_nix :: Bool
  , config :: Maybe FilePath
  , sub_command :: SubCommand
  } deriving Show

data SubCommand = BuildCommand BuildOpts
                | ProjectCommand ProjectOpts
                deriving Show

data BuildOpts = BuildOpts
  { infile :: FilePath
  , outfile :: FilePath
  } deriving Show

data ProjectOpts = ProjectOpts
  { project :: Maybe Text
  , command :: Maybe Text
  , list :: Bool
  , select :: Bool
  } deriving Show

data Backend = Fzf | Rofi deriving Show

default_options :: Options
default_options = Options
  { backend = Nothing
  , source_dirs = []
  , use_direnv = False
  , use_nix = False
  , config = Nothing
  , sub_command = ProjectCommand ProjectOpts
    { project = Nothing
    , command = Nothing
    , list = False
    , select = False
    }
  }

parser :: Parser Options
parser = Options
  <$> optional (opt parse_backend "backend" 'b' "Backend to use: fzf, rofi")
  <*> many (optPath "path" 'p' "Project directory")
  <*> switch "direnv" 'd' "Evaluate .envrc files using `direnv exec`"
  <*> switch "nix" 'n' "Invoke nix-shell if *.nix files are found"
  <*> optional (optPath "config" 'C' "Path to configuration file (default: ~/.config/envix)")
  <*> ( BuildCommand <$> subcommand "build" "Build custom envix" build_parser <|>
        ProjectCommand <$> subcommand "project" "Project actions" project_parser <|>
        ProjectCommand <$> project_parser)
  where
    parse_backend "fzf" = Just Fzf
    parse_backend "rofi" = Just Rofi
    parse_backend _ = Nothing

build_parser :: Parser BuildOpts
build_parser = BuildOpts
  <$> (fromMaybe "envix.hs" <$> optional (argPath "infile" "Input file (default: envix.hs)"))
  <*> (fromMaybe "envix"    <$> optional (argPath "outfile" "Output file (default: envix)"))

project_parser :: Parser ProjectOpts
project_parser = ProjectOpts
  <$> optional (argText "project" "Project to jump into")
  <*> optional (argText "command" "Command to run")
  <*> switch "list" 'l' "List projects"
  <*> switch "select" 's' "Select a project and output on stdout"

merge_opts :: ProjectOpts -> ProjectOpts -> ProjectOpts
merge_opts secondary primary = ProjectOpts
  { project = project primary <|> project secondary
  , command = command primary <|> command secondary
  , list = list primary
  , select = select primary
  }

-- | Read configuration from config file and command line arguments
parse_args :: IO (Either Text Options)
parse_args = runExceptT $ do
  opts <- Turtle.options "Launch project environment" parser
  let read_config = fmap Just <$> JSON.read_config (config opts)
  ExceptT read_config `catchE` handle_error opts >>= \case
    Nothing -> pure opts
    Just config -> pure opts
      { source_dirs = JSON.source_dirs config ++ source_dirs opts
      , use_direnv = JSON.use_direnv config || use_direnv opts
      , use_nix = JSON.use_nix config || use_nix opts
      }
  where handle_error opts NoSuchFile = case config opts of
          Nothing   -> pure Nothing
          Just path -> throwE (format ("No such file: "%fp) path)
        handle_error _ EmptyFile = pure Nothing
        handle_error _ (ParseError t) = throwE t
