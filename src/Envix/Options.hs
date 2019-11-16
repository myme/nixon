module Envix.Options
  ( Backend(..)
  , Options(..)
  , SubCommand(..)
  , BuildOpts(..)
  , ProjectOpts(..)
  , default_options
  , merge_opts
  , parse_args
  ) where

import           Control.Exception (catch)
import           Data.Maybe (fromMaybe)
import           Data.Text (Text)
import qualified Data.Text as T
import           Prelude hiding (FilePath)
import           System.Environment (withArgs)
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
  , source_dirs :: [FilePath]
  , list :: Bool
  , select :: Bool
  } deriving Show

data Backend = Fzf | Rofi deriving Show

default_options :: Options
default_options = Options
  { backend = Nothing
  , use_nix = False
  , config = Nothing
  , sub_command = ProjectCommand $ ProjectOpts
    { project = Nothing
    , command = Nothing
    , source_dirs = []
    , list = False
    , select = False
    }
  }

parser :: Parser Options
parser = Options
  <$> optional (opt parse_backend "backend" 'b' "Backend to use: fzf, rofi")
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
  <$> argPath "infile" "Input file"
  <*> argPath "outfile" "Output file"

project_parser :: Parser ProjectOpts
project_parser = ProjectOpts
  <$> optional (argText "project" "Project to jump into")
  <*> optional (argText "command" "Command to run")
  <*> many (optPath "path" 'p' "Project directory")
  <*> switch "list" 'l' "List projects"
  <*> switch "select" 's' "Select a project and output on stdout"

-- TODO: Filter comment lines (starting with '#')
-- TODO: Support quoted strings in e.g. file paths: `-p "this is a path"`
read_config :: FilePath -> IO [Text]
read_config path = T.words <$> (readTextFile path `catch` as_empty)
  where as_empty :: IOError -> IO Text
        as_empty _ = pure ""

merge_opts :: ProjectOpts -> ProjectOpts -> ProjectOpts
merge_opts secondary primary = ProjectOpts
  { project = project primary <|> project secondary
  , command = command primary <|> command secondary
  , source_dirs = source_dirs secondary ++ source_dirs primary
  , list = list primary
  , select = select primary
  }

-- | Read configuration from config file and command line arguments
parse_args :: IO Options
parse_args = do
  home' <- home
  cli_opts <- Turtle.options "Launch project environment" parser
  case sub_command cli_opts of
    BuildCommand _ -> pure cli_opts
    ProjectCommand project_opts -> do
      let config_file = fromMaybe (home' </> ".config/envix") (config cli_opts)
      file_args <- map T.unpack <$> read_config config_file
      if null file_args
        then pure cli_opts
        else do
          file_opts <- withArgs file_args (Turtle.options "config options" project_parser)
          pure $ cli_opts { sub_command = ProjectCommand $ merge_opts file_opts project_opts }
