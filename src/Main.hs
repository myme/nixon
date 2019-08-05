module Main where

import           Control.Exception (catch)
import           Data.Bool (bool)
import           Data.Maybe (fromMaybe)
import qualified Data.Text as T
import           Envix.Fzf
import           Envix.Projects
import           Envix.Rofi hiding (d, s)
import           Prelude hiding (FilePath)
import           System.Environment (withArgs)
import qualified System.IO as S
import           Turtle hiding (decimal, find, sort, sortBy)

type Selection = (Project, Command)
type Command = Text
type Commands = [(Text, Text)]

-- | Command line options.
data Options = Options { _project :: Maybe FilePath
                       , _backend :: Maybe Backend
                       , _source_dirs :: [FilePath]
                       , _command :: Maybe Text
                       , _no_nix :: Bool
                       , _config :: Maybe FilePath
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
  let config_file = fromMaybe (home' </> ".config/envix") (_config cli_opts)
  file_args <- map T.unpack <$> read_config config_file
  opts <- if null file_args
    then pure cli_opts
    else do
      file_opts <- withArgs file_args arg_parser
      pure $ Options { _project = _project cli_opts <|> _project file_opts
                     , _backend = _backend cli_opts <|> _backend file_opts
                     , _source_dirs = _source_dirs file_opts ++ _source_dirs cli_opts
                     , _command = _command cli_opts <|> _command file_opts
                     , _no_nix = _no_nix cli_opts || _no_nix file_opts
                     , _config = _config cli_opts
                     }
  if not . null $ _source_dirs opts
    then pure opts
    else pure $ opts { _source_dirs = ["~/src", "~/projects"] }

-- TODO: Launch terminal with nix-shell output if taking a long time.
-- If switching to a project takes a long time it would be nice to see a window
-- showing the progress of starting the environment.
main :: IO ()
main = do
  opts <- parse_args
  let source_dirs = _source_dirs opts
      -- TODO: Allow changing default command
      -- TODO: Allow format strings (%s) in commands to insert e.g. project path
      commands = [("konsole", "Terminal")
                 ,("emacs", "Editor")
                 ,("dolphin", "Files")
                 ]

  def_backend <- bool Rofi Fzf <$> S.hIsTerminalDevice S.stdin
  let backend = fromMaybe def_backend (_backend opts)
      (find, exec) = case backend of
        Fzf -> (fzf_projects, fzf_exec)
        Rofi -> (rofi_projects, rofi_exec)

  projects <- sort_projects <$> find_projects source_dirs
  action <- case _project opts of
    Nothing -> find commands projects
    Just project -> do
      let command = fst (head commands)
      matching <- resolve_project project source_dirs
      case matching of
        []  -> do
          printf ("No projects matching: " % fp % "\n") project
          pure Nothing
        [p] -> do
          path <- project_path <$> implode_home p
          printf ("Using matching project: " % fp % "\n") path
          pure $ Just (p, command)
        _   -> do
          printf ("Found multiple projects matching: " % fp % "\n") project
          find commands matching

  case action of
    Nothing -> putStrLn "No project selected."
    Just (project, command) -> exec command (_no_nix opts) project
