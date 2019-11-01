module Envix.Config
  ( Backend(..)
  , Options(..)
  , parse_args
  ) where

import           Control.Exception (catch)
import           Data.Maybe (fromMaybe)
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
data Options = Options { project :: Maybe FilePath
                       , command :: Maybe Text
                       , backend :: Maybe Backend
                       -- , backend_args :: [Text]
                       , source_dirs :: [FilePath]
                       , use_nix :: Bool
                       , config :: Maybe FilePath
                       , list :: Bool
                       , select :: Bool
                       } deriving Show

data Backend = Fzf | Rofi deriving Show

parser :: Parser Options
parser = Options
  <$> optional (argPath "project" "Project to jump into")
  <*> optional (argText "command" "Command to run")
  <*> optional (opt parse_backend "backend" 'b' "Backend to use: fzf, rofi")
  <*> many (optPath "path" 'p' "Project directory")
  <*> switch "nix" 'n' "Invoke nix-shell if *.nix files are found"
  <*> optional (optPath "config" 'C' "Path to configuration file (default: ~/.config/envix)")
  <*> switch "list" 'l' "List projects"
  <*> switch "select" 's' "Select a project and output on stdout"
  where parse_backend "fzf" = Just Fzf
        parse_backend "rofi" = Just Rofi
        parse_backend _ = Nothing

-- TODO: Filter comment lines (starting with '#')
-- TODO: Support quoted strings in e.g. file paths: `-p "this is a path"`
read_config :: FilePath -> IO [Text]
read_config path = T.words <$> (readTextFile path `catch` as_empty)
  where as_empty :: IOError -> IO Text
        as_empty _ = pure ""

merge_opts :: Options -> Options -> Options
merge_opts secondary primary = Options
  { project = project primary <|> project secondary
  , command = command primary <|> command secondary
  , backend = backend primary <|> backend secondary
  , source_dirs = source_dirs secondary ++ source_dirs primary
  , use_nix = use_nix primary || use_nix secondary
  , config = config primary
  , list = list primary
  , select = select primary
  }

-- | Read configuration from config file and command line arguments
parse_args :: IO Options
parse_args = do
  home' <- home
  let arg_parser = options "Launch project environment" parser
  cli_opts <- arg_parser
  let config_file = fromMaybe (home' </> ".config/envix") (config cli_opts)
  file_args <- map T.unpack <$> read_config config_file
  if null file_args
    then pure cli_opts
    else do
      file_opts <- withArgs file_args arg_parser
      pure $ merge_opts file_opts cli_opts
