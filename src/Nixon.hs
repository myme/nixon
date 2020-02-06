module Nixon
  ( nixon
  , nixon_with_config
  , default_config
  ) where

import           Control.Exception
import           Control.Monad.Trans.Maybe
import           Data.Maybe (fromMaybe)
import qualified Data.Text.IO as T
import           Nixon.Config.Options (Backend(..), BuildOpts, ProjectOpts, SubCommand(..))
import qualified Nixon.Config.Options as Options
import           Nixon.Config.Types (Config, LogLevel(..))
import qualified Nixon.Config.Types as Config
import           Nixon.Direnv
import           Nixon.Fzf
import           Nixon.Logging
import           Nixon.Nix
import           Nixon.Projects hiding (project_types)
import           Nixon.Projects.Defaults
import           Nixon.Projects.Types hiding (project_types)
import           Nixon.Rofi
import           Nixon.Select hiding (select)
import           Nixon.Types
import           Prelude hiding (FilePath, log)
import           Turtle hiding (decimal, env, err, find, shell, x)

-- | List projects, filtering if a filter is specified.
list :: [Project] -> Maybe Text -> IO ()
list projects query = do
  let fmt_line = fmap (text_to_line . format fp)
  paths <- fmt_line <$> traverse (implode_home . project_path) projects
  let fzf_opts = fzf_filter $ fromMaybe "" query
  fzf fzf_opts (Turtle.select paths) >>= \case
    Selection _ matching -> T.putStr matching
    _ -> printErr "No projects."

-- | Wrap GHC to build nixon with a custom config.
build_action :: BuildOpts -> IO ()
build_action opts = do
  let infile = format fp (Options.infile opts)
      outfile = format fp (Options.outfile opts)
      args = ["-Wall", "-threaded", "-rtsopts", "-with-rtsopts=-N", "-o", outfile, infile]
  code <- proc "ghc" args mempty
  case code of
    ExitFailure _ -> putStrLn "Compilation failed!"
    ExitSuccess -> putStrLn "Compilation successful!"

newtype NixonError = EmptyError Text deriving Show

instance Exception NixonError

fail_empty :: (MonadIO m) => Text -> m (Maybe a) -> m a
fail_empty err action = action >>= \case
  Nothing -> liftIO (throwIO $ EmptyError err)
  Just x -> pure x

-- | Maybe wrap a command in direnv/nix.
maybe_wrap_cmd :: Project -> Command -> Nixon Command
maybe_wrap_cmd project cmd = fmap (fromMaybe cmd) $ runMaybeT
   $  MaybeT (direnv_cmd cmd (project_path project))
  <|> MaybeT (nix_cmd cmd (project_path project))

-- | Find and run a command in a project.
run_cmd :: (Maybe Text -> Project -> IO (Maybe Command))
             -> Project
             -> ProjectOpts
             -> Selector
             -> Nixon ()
run_cmd find_command project opts selector = do
  cmd <- liftIO $ fail_empty "No command selected." $ find_command (Options.command opts) project
  cmd' <- maybe_wrap_cmd project cmd
  log_info (format ("Running command '"%w%"'") cmd')
  liftIO $ runSelect selector $ project_exec cmd' project

-- | Find/filter out a project and perform an action.
project_action :: [Project] -> ProjectOpts -> Nixon ()
project_action projects opts
  | Options.list opts = liftIO $ Nixon.list projects (Options.project opts)
  | otherwise = do
      env <- ask
      let ptypes = project_types env

          (find_project, find_command, selector) = case backend env of
            Fzf -> (fzf_projects, fzf_project_command, fzf_with_edit mempty)
            Rofi -> (rofi_projects, rofi_project_command, rofi mempty)

          -- TODO: Generalize rofi/fzf_projects and move this to Nixon.Projects using `select`
          find_project' (Just ".") = runMaybeT
             $  MaybeT (find_in_project ptypes =<< pwd)
            <|> MaybeT (find_project Nothing projects)
          find_project' query = find_project query projects

      project <- liftIO $ fail_empty "No project selected." $ find_project' (Options.project opts)
      if Options.select opts
        then liftIO $ printf (fp % "\n") (project_path project)
        else run_cmd find_command project opts selector

-- | Run a command from current directory
run_action :: ProjectOpts -> Nixon ()
run_action opts = do
  env <- ask
  let ptypes = project_types env
      (find_command, selector) = case backend env of
        Fzf -> (fzf_project_command, fzf_with_edit mempty)
        Rofi -> (rofi_project_command, rofi mempty)
  project <- liftIO $ do
    current <- from_path <$> pwd
    fromMaybe current <$> (find_in_project ptypes =<< pwd)
  run_cmd find_command project opts selector

-- TODO: Launch terminal with nix-shell output if taking a long time.
-- TODO: Allow changing default command
-- TODO: Project local commands (project/path/.nixon)
-- TODO: Pingbot integration?
-- If switching to a project takes a long time it would be nice to see a window
-- showing the progress of starting the environment.
nixon_with_config :: Config -> IO ()
nixon_with_config user_config = do
  opts <- either print_error pure =<< Options.parse_args
  err <- try $ runNixon opts user_config $ case Options.sub_command opts of
    BuildCommand build_opts -> do
      log_info "Running <build> command"
      liftIO (build_action build_opts)
    ProjectCommand project_opts -> do
      log_info "Running <project> command"
      env <- ask
      let ptypes = project_types env
          srcs = source_dirs env
      projects <- sort_projects <$> liftIO (find_projects 1 ptypes srcs)
      project_action projects project_opts
    RunCommand run_opts -> do
      log_info "Running <run> command"
      run_action run_opts
  case err of
    Left (EmptyError msg) -> print_error msg
    Right _ -> pure ()
  where print_error err = printErr err >> exit (ExitFailure 1)

default_config :: Config
default_config = Config.Config
  { Config.backend = Nothing
  , Config.project_types = default_projects
  , Config.source_dirs = []
  , Config.use_direnv = Nothing
  , Config.use_nix = Nothing
  , Config.loglevel = LogWarning
  }

-- | Nixon with default configuration
nixon :: IO ()
nixon = nixon_with_config default_config
