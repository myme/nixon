module Nixon
  ( nixon
  , nixon_with_config
  , default_config
  ) where

import           Control.Monad.Trans.Class
import           Control.Monad.Trans.Except
import           Control.Monad.Trans.Maybe
import           Data.Bool (bool)
import           Data.Maybe (fromMaybe)
import qualified Data.Text.IO as T
import           Nixon.Config as Config
import           Nixon.Config.Options (Backend(..), BuildOpts, ProjectOpts, SubCommand(..))
import qualified Nixon.Config.Options as Options
import           Nixon.Direnv
import           Nixon.Fzf
import           Nixon.Logging
import           Nixon.Nix
import           Nixon.Projects hiding (project_types)
import           Nixon.Projects.Defaults
import           Nixon.Projects.Types hiding (project_types)
import           Nixon.Rofi
import           Nixon.Select hiding (select)
import           Prelude hiding (FilePath, log)
import qualified System.IO as IO
import           Turtle hiding (decimal, err, find, shell)

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

on_empty :: (Monad m) => Text -> m (Maybe a) -> ExceptT Text m a
on_empty err = maybeToExceptT err . MaybeT

handleExcept :: (MonadIO m) => ExceptT Text m () -> m ()
handleExcept action = runExceptT action >>= \case
  Left err -> printErr err >> exit (ExitFailure 1)
  Right _ -> pure ()

get_backend :: Nixon Backend
get_backend = do
  config <- ask
  def_backend <- liftIO (bool Rofi Fzf <$> IO.hIsTerminalDevice IO.stdin)
  pure $ fromMaybe def_backend (Config.backend config)

-- | Maybe wrap a command in direnv/nix.
maybe_wrap_cmd :: Config -> Project -> Command -> IO Command
maybe_wrap_cmd config project cmd = fmap (fromMaybe cmd) $ runMaybeT
   $  MaybeT (direnv_cmd config cmd (project_path project))
  <|> MaybeT (nix_cmd config cmd (project_path project))

-- | Find/filter out a project and perform an action.
project_action :: [Project] -> ProjectOpts -> Nixon ()
project_action projects opts
  | Options.list opts = liftIO $ Nixon.list projects (Options.project opts)
  | otherwise = do
      config <- ask
      backend <- get_backend
      let ptypes = Config.project_types config

          (find_project, find_command, selector) = case backend of
            Fzf -> (fzf_projects, fzf_project_command, fzf_with_edit mempty)
            Rofi -> (rofi_projects, rofi_project_command, rofi mempty)

          -- TODO: Generalize rofi/fzf_projects and move this to Nixon.Projects using `select`
          find_project' (Just ".") = runMaybeT
             $  MaybeT (find_in_project ptypes =<< pwd)
            <|> MaybeT (find_project Nothing projects)
          find_project' query = find_project query projects

      handleExcept $ do
        project <- on_empty "No project selected." $ liftIO $ find_project' (Options.project opts)
        if Options.select opts
          then liftIO $ printf (fp % "\n") (project_path project)
          else do
            cmd <- on_empty "No command selected." $ liftIO $ find_command (Options.command opts) project
            cmd' <- liftIO $ maybe_wrap_cmd config project cmd
            lift $ log_info (format ("Running command '"%w%"'") cmd')
            liftIO $ runSelect selector $ project_exec cmd' project

-- | Run a command from current directory
run_action :: ProjectOpts -> Nixon ()
run_action opts = do
  config <- ask
  backend <- get_backend
  let ptypes = Config.project_types config
      (find_command, selector) = case backend of
        Fzf -> (fzf_project_command, fzf_with_edit mempty)
        Rofi -> (rofi_project_command, rofi mempty)
  handleExcept $ do
    project <- liftIO $ do
      current <- from_path <$> pwd
      fromMaybe current <$> (find_in_project ptypes =<< pwd)
    cmd <- on_empty "No command selected." $ liftIO $ find_command (Options.command opts) project
    liftIO $ do
      cmd' <- maybe_wrap_cmd config project cmd
      runSelect selector $ project_exec cmd' project

-- TODO: Launch terminal with nix-shell output if taking a long time.
-- TODO: Allow changing default command
-- TODO: Project local commands (project/path/.nixon)
-- TODO: Pingbot integration?
-- If switching to a project takes a long time it would be nice to see a window
-- showing the progress of starting the environment.
nixon_with_config :: Config -> IO ()
nixon_with_config user_config = do
  opts <- either print_error pure =<< Options.parse_args
  let config = build_config opts user_config
  runNixon config $ case Options.sub_command opts of
    BuildCommand build_opts -> do
      log_info "Running <build> command"
      liftIO (build_action build_opts)
    ProjectCommand project_opts -> do
      log_info "Running <project> command"
      let ptypes = Config.project_types config
          srcs = Config.source_dirs config
      projects <- sort_projects <$> liftIO (find_projects 1 ptypes srcs)
      project_action projects project_opts
    RunCommand run_opts -> do
      log_info "Running <run> command"
      run_action run_opts
  where print_error err = printErr err >> exit (ExitFailure 1)

default_config :: Config
default_config = Config { backend = Nothing
                        , project_types = default_projects
                        , source_dirs = []
                        , use_direnv = False
                        , use_nix = False
                        , loglevel = LogWarning
                        }

-- | Nixon with default configuration
nixon :: IO ()
nixon = nixon_with_config default_config
