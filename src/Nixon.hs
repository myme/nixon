module Nixon
  ( nixon
  , nixon_with_config
  ) where

import           Control.Exception hiding (evaluate)
import           Control.Monad.Trans.Maybe
import           Control.Monad.Trans.Reader
import           Data.Aeson
import           Data.Foldable (find)
import           Data.List (intersect)
import           Data.Maybe (fromMaybe, catMaybes)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import           Nixon.Command (Command(..), CommandEnv(..), CommandOutput(..), show_command_oneline)
import qualified Nixon.Config as Config
import           Nixon.Config.Options (Backend(..), CompletionType, ProjectOpts(..), RunOpts(..), SubCommand(..))
import qualified Nixon.Config.Options as Opts
import           Nixon.Config.Types (ignore_case)
import qualified Nixon.Config.Types as Config
import           Nixon.Evaluator
import           Nixon.Fzf
import           Nixon.Logging
import           Nixon.Process (Env, run_with_output)
import qualified Nixon.Project as P
import           Nixon.Project (Project, ProjectType(..), project_path)
import           Nixon.Rofi
import           Nixon.Select (Selection(..), Selector)
import qualified Nixon.Select as Select
import           Nixon.Types hiding (Env)
import           Nixon.Utils
import           Prelude hiding (FilePath, log)
import           System.Environment (withArgs)
import           Turtle hiding (decimal, die, env, err, find, output, shell, text, x)
import qualified Turtle.Bytes as BS

-- | List projects, filtering if a filter is specified.
list_projects :: [Project] -> Maybe Text -> Nixon ()
list_projects projects query = do
  let fmt_line = fmap (Select.Identity . format fp)
  paths <- liftIO $ fmt_line <$> traverse (implode_home . project_path) projects
  let fzf_opts = fzf_filter $ fromMaybe "" query
  liftIO (fzf fzf_opts (Turtle.select paths)) >>= \case
    Selection _ matching -> liftIO $ T.putStr matching
    _ -> log_error "No projects."

fail_empty :: (MonadIO m) => Text -> m (Maybe a) -> m a
fail_empty err action = action >>= \case
  Nothing -> liftIO (throwIO $ EmptyError err)
  Just x -> pure x

-- | Attempt to parse a local JSON
with_local_config :: Project -> Nixon () -> Nixon ()
with_local_config project action = do
  liftIO (Config.find_local_config (project_path project)) >>= \case
    Nothing -> action
    Just cfg -> local (\env -> env { config = config env <> cfg }) action

list_commands :: Project -> Nixon [Command]
list_commands project = filter filter_cmd . commands . config <$> ask
  where ptypes = map project_id $ P.project_types project
        filter_cmd cmd = let ctypes = cmdProjectTypes cmd
                         in null ctypes || not (null $ intersect ptypes ctypes)

-- | Find and run a command in a project.
run_cmd :: CommandSelector
        -> Project
        -> RunOpts
        -> Selector Nixon
        -> Nixon ()
run_cmd select_command project opts selector = with_local_config project $ do
  cmds <- list_commands project
  cmd <- liftIO $ fail_empty "No command selected." $ select_command project opts cmds
  if Opts.run_select opts
    then do
      resolved <- resolve_cmd project selector cmd
      printf (s%"\n") resolved
    else do
      let project_selector = \shell' -> cd (project_path project) >> selector shell'
      env' <- resolve_env project project_selector cmd
      evaluate cmd (Just $ project_path project) env'

-- | Resolve all command environment variable placeholders.
resolve_env :: Project -> Selector Nixon -> Command -> Nixon Env
resolve_env project selector cmd = mapM resolve_each (cmdEnv cmd)
  where
    resolve_each (name, Env cmd') = (name,) <$> (assert_command cmd' >>= resolve_cmd project selector)
    assert_command cmd_name = do
      cmd' <- find ((==) cmd_name . cmdName) . commands . config <$> ask
      maybe (error $ "Invalid argument: " <> T.unpack cmd_name) pure cmd'

-- | Resolve command to selectable output.
resolve_cmd :: Project -> Selector Nixon -> Command -> Nixon Text
resolve_cmd project selector cmd = do
  env' <- resolve_env project selector cmd
  let path' = Just $ project_path project
  linesEval <- getEvaluator (run_with_output stream) cmd path' env'
  jsonEval  <- getEvaluator (run_with_output BS.stream) cmd path' env'
  selection <- selector $ do
    case cmdOutput cmd of
      Lines -> Select.Identity . lineToText <$> linesEval
      JSON -> do
        output <- BS.strict jsonEval
        case eitherDecodeStrict output :: Either String [Select.Candidate] of
          Left err -> error err
          Right candidates -> select candidates
  case selection of
    Selection _ result -> pure result
    _ -> error "Argument expansion aborted"

type ProjectSelector = Maybe Text -> [Project] -> IO (Maybe Project)
type CommandSelector = Project -> RunOpts -> [Command] -> IO (Maybe Command)

get_selectors :: Nixon ([ProjectType], ProjectSelector, CommandSelector, Selector Nixon)
get_selectors = do
  env <- ask
  let cfg = config env
      ptypes = project_types cfg
      fzf_opts = mconcat $ catMaybes [fzf_exact <$> exact_match cfg, fzf_ignore_case <$> ignore_case cfg]
      rofi_opts = mconcat $ catMaybes [rofi_exact <$> exact_match cfg, rofi_ignore_case <$> ignore_case cfg]
  pure $ case backend env of
    Fzf -> (ptypes, fzf_projects fzf_opts, fzf_project_command fzf_opts, fzf_with_edit fzf_opts)
    Rofi -> (ptypes, rofi_projects rofi_opts, \_ -> rofi_project_command rofi_opts, rofi rofi_opts)

-- | Find/filter out a project and perform an action.
project_action :: [Project] -> ProjectOpts -> Nixon ()
project_action projects opts
  | Opts.proj_list opts = list_projects projects (Opts.proj_project opts)
  | otherwise = do
      (ptypes, find_project, find_command, selector) <- get_selectors

      let find_project' (Just ".") = runMaybeT
             $  MaybeT (P.find_in_project ptypes =<< pwd)
            <|> MaybeT (find_project Nothing projects)
          find_project' query = find_project query projects

      project <- liftIO $ fail_empty "No project selected." $ find_project' (Opts.proj_project opts)
      if Opts.proj_select opts
        then liftIO $ printf (fp % "\n") (project_path project)
        else
          let opts' = RunOpts (Opts.proj_command opts) (Opts.proj_list opts) (Opts.proj_select opts)
          in run_cmd find_command project opts' selector

-- | Run a command from current directory
run_action :: RunOpts -> Nixon ()
run_action opts = do
  (ptypes, _project, find_command, selector) <- get_selectors
  project <- liftIO (P.find_in_project_or_default ptypes =<< pwd)
  if Opts.run_list opts
    then list_commands project >>= liftIO . mapM_ (T.putStrLn . show_command_oneline)
    else run_cmd find_command project opts selector

die :: (Show a, MonadIO m) => a -> m b
die err = liftIO $ log_error (format w err) >> exit (ExitFailure 1)

-- If switching to a project takes a long time it would be nice to see a window
-- showing the progress of starting the environment.
nixon_with_config :: MonadIO m => Config.Config -> m ()
nixon_with_config user_config = liftIO $ do
  (sub_cmd, cfg) <- either die pure =<< Opts.parse_args (nixon_completer user_config)
  err <- try $ runNixon (user_config <> cfg) $ case sub_cmd of
    ProjectCommand project_opts -> do
      log_info "Running <project> command"
      cfg' <- config <$> ask
      let ptypes = project_types cfg'
          srcs = project_dirs cfg'
      projects <- P.sort_projects <$> liftIO (P.find_projects 1 ptypes srcs)
      project_action projects project_opts
    RunCommand run_opts -> do
      log_info "Running <run> command"
      run_action run_opts
  case err of
    Left (EmptyError msg) -> die msg
    Right _ -> pure ()

nixon_completer :: Config.Config -> CompletionType -> [String] -> IO [String]
nixon_completer user_config comp_type args = do
  let parse_args = Opts.parse_args $ nixon_completer user_config
  (_, cfg) <- either die pure =<< withArgs args parse_args
  runNixon (user_config <> cfg) $ do
    cfg' <- config <$> ask
    let ptypes = project_types cfg'
        srcs = project_dirs cfg'
    projects <- P.sort_projects <$> liftIO (P.find_projects 1 ptypes srcs)
    case comp_type of
      Opts.Project -> pure $ map (T.unpack . format fp . P.project_name) projects
      Opts.Run -> do
        project <- case args of
          ("project":p:_) -> do
            current <- P.from_path <$> pwd
            let p' = find ((==) p . T.unpack . format fp . P.project_name) projects
            pure $ fromMaybe current p'
          _ -> liftIO $ P.find_in_project_or_default ptypes =<< pwd
        commands <- list_commands project
        pure $ map (T.unpack . cmdName) commands

-- | Nixon with default configuration
nixon :: MonadIO m => m ()
nixon = nixon_with_config Config.defaultConfig
