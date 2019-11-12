module Main where

import           Data.Bool (bool)
import           Data.Function (on)
import           Data.List (find)
import           Data.Maybe (fromMaybe)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Envix.Config as Opts
import           Envix.Fzf
import           Envix.Projects
import           Envix.Rofi
import           Prelude hiding (FilePath)
import qualified System.IO as IO
import           Turtle hiding (decimal, find, sort, shell, sortBy)

-- | Print a text message to stderr
printErr :: Text -> IO ()
printErr = T.hPutStrLn IO.stderr

-- | List projects, filtering if a filter is specified.
list :: [Project] -> Opts.Options -> IO ()
list projects opts = do
  paths <- fmap (format fp) <$> traverse (implode_home . project_path) projects
  let fzf_opts = fzf_filter $ fromMaybe "" (Opts.project opts)
  fzf fzf_opts paths >>= \case
    FzfSelection _ matching -> T.putStr matching
    _ -> printErr "No projects."

-- | Find/filter out a project in which path is a subdirectory.
find_in_project :: [Project] -> FilePath -> Maybe Project
find_in_project projects path = find (is_prefix . project_path) projects
  where is_prefix project = (T.isPrefixOf `on` format fp) project path

-- | Find/filter out a project and perform an action.
projectAction :: [Project] -> Opts.Options -> IO ()
projectAction projects opts = do
  def_backend <- bool Opts.Rofi Opts.Fzf <$> IO.hIsTerminalDevice IO.stdin
  let backend = fromMaybe def_backend (Opts.backend opts)

      (find_project, find_command, exec) = case backend of
        Opts.Fzf -> (fzf_projects, fzf_project_command, fzf_exec)
        Opts.Rofi -> (rofi_projects, rofi_project_command, rofi_exec)

      find_project' :: Maybe Text -> IO (Maybe Project)
      find_project' query
        | query == Just "." = find_in_project projects <$> pwd >>= \case
            Nothing  -> find_project Nothing projects
            project' -> return project'
        | otherwise = find_project query projects

  find_project' (Opts.project opts) >>= \case
    Nothing -> do
      printErr "No project selected."
      exit (ExitFailure 1)
    Just project'
      | Opts.select opts -> printf (fp % "\n") (project_path project')
      | otherwise -> do
          cmd <- find_command (Opts.command opts) project'
          case cmd of
            Nothing -> do
              printErr "No command selected."
              exit (ExitFailure 1)
            cmd' -> exec cmd' (Opts.use_nix opts) project'

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
      -- TODO: Project local commands (project/path/.envix)
      -- TODO: Pingbot integration?

  projects <- sort_projects <$> find_projects source_dirs

  if Opts.list opts
    then list projects opts
    else projectAction projects opts
