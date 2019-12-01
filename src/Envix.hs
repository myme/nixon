module Envix
  ( envix
  , envix_with_config
  , default_config
  ) where

import           Control.Monad.Trans.Except
import           Control.Monad.Trans.Maybe
import           Data.Bool (bool)
import           Data.Maybe (fromMaybe)
import qualified Data.Text.IO as T
import           Envix.Config as Config
import           Envix.Fzf
import           Envix.Config.Options (Backend(..), BuildOpts, ProjectOpts, SubCommand(..))
import qualified Envix.Config.Options as Options
import           Envix.Direnv
import           Envix.Nix
import           Envix.Projects hiding (project_types)
import           Envix.Projects.Defaults
import           Envix.Projects.Types hiding (project_types)
import           Envix.Rofi
import           Envix.Select hiding (select)
import           Prelude hiding (FilePath)
import qualified System.IO as IO
import           Turtle hiding (decimal, err, find, shell)

-- | Print a text message to stderr
printErr :: (MonadIO m) => Text -> m ()
printErr = liftIO . T.hPutStrLn IO.stderr

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

on_empty :: (Monad m) => Text -> m (Maybe a) -> ExceptT Text m a
on_empty err = maybeToExceptT err . MaybeT

handleExcept :: (MonadIO m) => ExceptT Text m () -> m ()
handleExcept action = runExceptT action >>= \case
  Left err -> printErr err >> exit (ExitFailure 1)
  Right _ -> pure ()

get_backend :: Config -> IO Backend
get_backend config = do
  def_backend <- bool Rofi Fzf <$> IO.hIsTerminalDevice IO.stdin
  pure $ fromMaybe def_backend (Config.backend config)

-- | Find/filter out a project and perform an action.
project_action :: Config -> [Project] -> ProjectOpts -> IO ()
project_action config projects opts
  | Options.list opts = Envix.list projects (Options.project opts)
  | otherwise = do
      backend <- get_backend config
      let ptypes = Config.project_types config

          (find_project, find_command, selector) = case backend of
            Fzf -> (fzf_projects, fzf_project_command, fzf_with_edit mempty)
            Rofi -> (rofi_projects, rofi_project_command, rofi mempty)

          -- TODO: Generalize rofi/fzf_projects and move this to Envix.Projects using `select`
          find_project' (Just ".") = runMaybeT
             $  MaybeT (find_in_project ptypes =<< pwd)
            <|> MaybeT (find_project Nothing projects)
          find_project' query = find_project query projects

      handleExcept $ do
        project <- on_empty "No project selected." $ find_project' (Options.project opts)
        if Options.select opts
          then printf (fp % "\n") (project_path project)
          else do
            cmd <- on_empty "No command selected." $ find_command (Options.command opts) project
            liftIO $ do
              cmd' <- fmap (fromMaybe cmd) $ runMaybeT $
                MaybeT (direnv_cmd config cmd (project_path project)) <|>
                MaybeT (nix_cmd config cmd (project_path project))
              runSelect selector $ project_exec cmd' project

-- | Run a command from current directory
run_action :: Config -> ProjectOpts -> IO ()
run_action config opts = do
  backend <- get_backend config
  let ptypes = Config.project_types config
      (find_command, selector) = case backend of
        Fzf -> (fzf_project_command, fzf_with_edit mempty)
        Rofi -> (rofi_project_command, rofi mempty)
  handleExcept $ do
    project <- liftIO $ do
      current <- from_path <$> pwd
      fromMaybe current <$> (find_in_project ptypes =<< pwd)
    cmd <- on_empty "No command selected." $ find_command (Options.command opts) project
    liftIO $ do
      cmd' <- fmap (fromMaybe cmd) $ runMaybeT $
        MaybeT (direnv_cmd config cmd (project_path project)) <|>
        MaybeT (nix_cmd config cmd (project_path project))
      runSelect selector $ project_exec cmd' project

-- TODO: Launch terminal with nix-shell output if taking a long time.
-- TODO: Allow changing default command
-- TODO: Project local commands (project/path/.envix)
-- TODO: Pingbot integration?
-- If switching to a project takes a long time it would be nice to see a window
-- showing the progress of starting the environment.
envix_with_config :: Config -> IO ()
envix_with_config user_config = do
  opts <- either print_error pure =<< Options.parse_args
  let config = build_config opts user_config
  case Options.sub_command opts of
    BuildCommand build_opts -> build_action build_opts
    ProjectCommand project_opts -> do
      let ptypes = Config.project_types config
          srcs = Config.source_dirs config
      projects <- sort_projects <$> find_projects 1 ptypes srcs
      project_action config projects project_opts
    RunCommand run_opts -> run_action config run_opts
  where print_error err = printErr err >> exit (ExitFailure 1)

default_config :: Config
default_config = Config { backend = Nothing
                        , project_types = default_projects
                        , source_dirs = []
                        , use_direnv = False
                        , use_nix = False
                        }

-- | Envix with default configuration
envix :: IO ()
envix = envix_with_config default_config
