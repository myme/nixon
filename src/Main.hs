module Main where

import           Data.Bool (bool)
import           Data.Maybe (fromMaybe)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Envix.Config as Opts
import           Envix.Fzf
import           Envix.Projects
import           Envix.Rofi hiding (d, s)
import           Prelude hiding (FilePath)
import qualified System.IO as S
import           Turtle hiding (decimal, find, sort, sortBy)

type Selection = (Project, Command)
type Command = Text
type Commands = [(Text, Text)]

-- TODO: Integrate with `direnv`
-- TODO: Launch terminal with nix-shell output if taking a long time.
-- If switching to a project takes a long time it would be nice to see a window
-- showing the progress of starting the environment.
main :: IO ()
main = do
  opts <- Opts.parse_args
  let source_dirs = Opts.source_dirs opts
      -- TODO: Allow changing default command
      -- TODO: Allow format strings (%s) in commands to insert e.g. project path
      commands = [("konsole", "Terminal")
                 ,("emacs", "Editor")
                 ,("dolphin", "Files")
                 ]

  def_backend <- bool Opts.Rofi Opts.Fzf <$> S.hIsTerminalDevice S.stdin
  let backend = fromMaybe def_backend (Opts.backend opts)
      (find, exec) = case backend of
        Opts.Fzf -> (fzf_projects, fzf_exec)
        Opts.Rofi -> (rofi_projects, rofi_exec)

  projects <- sort_projects <$> find_projects source_dirs
  if Opts.list opts
    then do
      projects' <- map fst <$> traverse fzf_format_project_name projects
      T.putStr $ T.unlines projects'

    else do
      action <- case Opts.project opts of
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
        Just (project, command) -> exec command (Opts.no_nix opts) project
