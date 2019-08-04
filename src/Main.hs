module Main where

import           Data.Bool (bool)
import           Data.Maybe (fromMaybe)
import           Envix.Fzf
import           Envix.Projects
import           Envix.Rofi hiding (d, s)
import           Prelude hiding (FilePath)
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
                       }

data Backend = Fzf | Rofi

parser :: Parser Options
parser = Options
  <$> optional (argPath "project" "Project to jump into")
  <*> optional (opt parse_backend "backend" 'b' "Backend to use: fzf, rofi")
  <*> many (optPath "path" 'p' "Project directory")
  <*> optional (optText "command" 'c' "Command to run")
  <*> switch "no-nix" 'n' "Do not invoke nix-shell if *.nix files are found"
  where parse_backend "fzf" = Just Fzf
        parse_backend "rofi" = Just Rofi
        parse_backend _ = Nothing

-- TODO: Launch terminal with nix-shell output if taking a long time.
-- If switching to a project takes a long time it would be nice to see a window
-- showing the progress of starting the environment.
main :: IO ()
main = do
  opts <- options "Launch project environments" parser
  let source_dirs = if null (_source_dirs opts)
        then ["~/src", "~/projects"]
        else _source_dirs opts
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
