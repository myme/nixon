module Envix
  ( envix
  , envix_with_config
  ) where

import           Data.Bool (bool)
import           Data.Maybe (fromMaybe)
import qualified Data.Text.IO as T
import           Envix.Config as Config
import           Envix.Fzf
import           Envix.Options as Options
import           Envix.Projects
import           Envix.Projects.Types
import           Envix.Rofi
import           Envix.Select hiding (select)
import           Prelude hiding (FilePath)
import qualified System.IO as IO
import           Turtle hiding (decimal, find, sort, shell, sortBy)

-- | Print a text message to stderr
printErr :: Text -> IO ()
printErr = T.hPutStrLn IO.stderr

-- | List projects, filtering if a filter is specified.
list :: [Project] -> Maybe Text -> IO ()
list projects query = do
  let fmt_line = fmap (text_to_line . format fp)
  paths <- fmt_line <$> traverse (implode_home . project_path) projects
  let fzf_opts = fzf_filter $ fromMaybe "" query
  fzf fzf_opts (Turtle.select paths) >>= \case
    Selection _ matching -> T.putStr matching
    _ -> printErr "No projects."

-- | Wrap GHC to build envix with a custom config.
build_action :: BuildOpts -> IO ()
build_action opts = do
  let infile = format fp (Options.infile opts)
      outfile = format fp (Options.outfile opts)
      args = ["-Wall", "-threaded", "-rtsopts", "-with-rtsopts=-N", "-o", outfile, infile]
  code <- proc "ghc" args mempty
  case code of
    ExitFailure _ -> putStrLn "Compilation failed!"
    ExitSuccess -> putStrLn "Compilation successful!"

-- | Find/filter out a project and perform an action.
project_action :: [Project] -> [ProjectType] -> Maybe Backend -> Options -> ProjectOpts -> IO ()
project_action projects project_types backendM opts popts = do
  def_backend <- bool Rofi Fzf <$> IO.hIsTerminalDevice IO.stdin
  let backend = fromMaybe def_backend backendM

      (find_project, find_command, exec) = case backend of
        Fzf -> (fzf_projects, fzf_project_command, fzf_exec)
        Rofi -> (rofi_projects, rofi_project_command, rofi_exec)

      find_project' :: Maybe Text -> IO (Maybe Project)
      find_project' query
        | query == Just "." = pwd >>= find_in_project project_types >>= \case
            Nothing  -> find_project Nothing projects
            project' -> pure project'
        | otherwise = find_project query projects

  find_project' (Options.project popts) >>= \case
    Nothing -> do
      printErr "No project selected."
      exit (ExitFailure 1)
    Just project'
      | Options.select popts -> printf (fp % "\n") (project_path project')
      | otherwise -> do
          cmd <- find_command (Options.command popts) project'
          case cmd of
            Nothing -> do
              printErr "No command selected."
              exit (ExitFailure 1)
            Just cmd' -> if use_direnv opts
              then let parts = command_parts cmd'
                       dir = TextPart $ format fp $ project_path project'
                       parts' = [TextPart "direnv exec", dir, head parts] ++ tail parts
                   in exec (cmd' { command_parts = parts' }) project'
              else exec cmd' project'

-- TODO: Integrate with `direnv`: direnv exec CMD [ARGS...]
-- TODO: Launch terminal with nix-shell output if taking a long time.
-- TODO: Allow changing default command
-- TODO: Project local commands (project/path/.envix)
-- TODO: Pingbot integration?
-- If switching to a project takes a long time it would be nice to see a window
-- showing the progress of starting the environment.
envix_with_config :: Config -> IO ()
envix_with_config config = do
  opts <- Options.parse_args
  case Options.sub_command opts of
    BuildCommand build_opts -> build_action build_opts
    ProjectCommand project_opts -> do
      let ptypes = Config.project_types config
          srcs = Options.source_dirs project_opts
      projects <- sort_projects <$> find_projects 1 ptypes srcs
      if Options.list project_opts
        then Envix.list projects (Options.project project_opts)
        else project_action projects ptypes (Options.backend opts) opts project_opts


-- | Envix with default configuration
envix :: IO ()
envix = envix_with_config default_config
