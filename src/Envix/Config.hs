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
import           Turtle

-- TODO: Add CLI opt for outputting bash/zsh completion script.
-- TODO: Add CLI opt for listing out projects (no action).
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
                       , backend :: Maybe Backend
                       -- , backend_args :: [Text]
                       , source_dirs :: [FilePath]
                       , command :: Maybe Text
                       , no_nix :: Bool
                       , config :: Maybe FilePath
                       } deriving Show

data Backend = Fzf | Rofi deriving Show

parser :: Parser Options
parser = Options
  <$> optional (argPath "project" "Project to jump into")
  <*> optional (opt parse_backend "backend" 'b' "Backend to use: fzf, rofi")
  <*> many (optPath "path" 'p' "Project directory")
  <*> optional (optText "command" 'c' "Command to run")
  <*> switch "no-nix" 'n' "Do not invoke nix-shell if *.nix files are found"
  <*> optional (optPath "config" 'C' "Path to configuration file (default: ~/.config/envix)")
  where parse_backend "fzf" = Just Fzf
        parse_backend "rofi" = Just Rofi
        parse_backend _ = Nothing

-- TODO: Filter comment lines (starting with '#')
-- TODO: Support quoted strings in e.g. file paths: `-p "this is a path"`
read_config :: FilePath -> IO [Text]
read_config path = T.words <$> (readTextFile path `catch` as_empty)
  where as_empty :: IOError -> IO Text
        as_empty _ = pure ""

-- | Read configuration from config file and command line arguments
parse_args :: IO Options
parse_args = do
  home' <- home
  let arg_parser = options "Launch project environment" parser
  cli_opts <- arg_parser
  let config_file = fromMaybe (home' </> ".config/envix") (config cli_opts)
  file_args <- map T.unpack <$> read_config config_file
  opts <- if null file_args
    then pure cli_opts
    else do
      file_opts <- withArgs file_args arg_parser
      pure $ Options { project = project cli_opts <|> project file_opts
                     , backend = backend cli_opts <|> backend file_opts
                     , source_dirs = source_dirs file_opts ++ source_dirs cli_opts
                     , command = command cli_opts <|> command file_opts
                     , no_nix = no_nix cli_opts || no_nix file_opts
                     , config = config cli_opts
                     }
  if not . null $ source_dirs opts
    then pure opts
    else pure $ opts { source_dirs = ["~/src", "~/projects"] }
