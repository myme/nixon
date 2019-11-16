module Envix
  ( envix
  , envixWithConfig
  ) where

import           Data.Bool (bool)
import           Data.Maybe (fromMaybe)
import qualified Data.Text.IO as T
import           Envix.Config as Config
import           Envix.Fzf
import           Envix.Options as Options
import           Envix.Projects
import           Envix.Rofi
import           Envix.Select hiding (select)
import           Prelude hiding (FilePath)
import qualified System.IO as IO
import           Turtle hiding (decimal, find, sort, shell, sortBy)

-- | Print a text message to stderr
printErr :: Text -> IO ()
printErr = T.hPutStrLn IO.stderr

-- | List projects, filtering if a filter is specified.
list :: [Project] -> Options -> IO ()
list projects opts = do
  let fmt_line = fmap (text_to_line . format fp)
  paths <- fmt_line <$> traverse (implode_home . project_path) projects
  let fzf_opts = fzf_filter $ fromMaybe "" (project opts)
  fzf fzf_opts (Turtle.select paths) >>= \case
    Selection _ matching -> T.putStr matching
    _ -> printErr "No projects."

-- | Find/filter out a project and perform an action.
projectAction :: [Project] -> Options -> IO ()
projectAction projects opts = do
  def_backend <- bool Rofi Fzf <$> IO.hIsTerminalDevice IO.stdin
  let backend = fromMaybe def_backend (Options.backend opts)

      (find_project, find_command, exec) = case backend of
        Fzf -> (fzf_projects, fzf_project_command, fzf_exec)
        Rofi -> (rofi_projects, rofi_project_command, rofi_exec)

      find_project' :: Maybe Text -> IO (Maybe Project)
      find_project' query
        | query == Just "." = find_in_project projects <$> pwd >>= \case
            Nothing  -> find_project Nothing projects
            project' -> pure project'
        | otherwise = find_project query projects

  find_project' (Options.project opts) >>= \case
    Nothing -> do
      printErr "No project selected."
      exit (ExitFailure 1)
    Just project'
      | Options.select opts -> printf (fp % "\n") (project_path project')
      | otherwise -> do
          cmd <- find_command (Options.command opts) project'
          case cmd of
            Nothing -> do
              printErr "No command selected."
              exit (ExitFailure 1)
            Just cmd' -> exec cmd' project'

-- TODO: Integrate with `direnv`: direnv exec CMD [ARGS...]
-- TODO: Launch terminal with nix-shell output if taking a long time.
-- TODO: Allow changing default command
-- TODO: Allow format strings (%s) in commands to insert e.g. project path
-- TODO: Project local commands (project/path/.envix)
-- TODO: Pingbot integration?
-- If switching to a project takes a long time it would be nice to see a window
-- showing the progress of starting the environment.
envixWithConfig :: Config -> IO ()
envixWithConfig config = do
  opts <- Options.parse_args
  projects <- sort_projects <$> find_projects (
    config { Config.options = merge_opts (Config.options config) opts })
  if Options.list opts
    then Envix.list projects opts
    else projectAction projects opts


envix :: IO ()
envix = envixWithConfig default_config
