module Nixon
  ( nixon,
    nixon_with_config,
  )
where

import Control.Exception (throwIO, try)
import Control.Monad.Catch (MonadMask)
import Control.Monad.Trans.Reader (ask, local)
import Data.Aeson (eitherDecodeStrict)
import Data.Foldable (find)
import Data.List (intersect)
import Data.List.NonEmpty (NonEmpty ((:|)))
import Data.Maybe (fromMaybe)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Nixon.Backend (Backend)
import qualified Nixon.Backend as Backend
import Nixon.Backend.Fzf
  ( fzf,
    fzfBackend,
    fzfFilter,
  )
import Nixon.Backend.Rofi (rofiBackend)
import Nixon.Command (Command (..), CommandEnv (..), CommandOutput (..), show_command, show_command_with_description)
import qualified Nixon.Command as Cmd
import qualified Nixon.Config as Config
import Nixon.Config.Options (BackendType (..), CompletionType, ProjectOpts (..), RunOpts (..), SubCommand (..))
import qualified Nixon.Config.Options as Opts
import qualified Nixon.Config.Types as Config
import Nixon.Evaluator (evaluate, getEvaluator)
import Nixon.Logging (log_error, log_info)
import Nixon.Process (Env, run, run_with_output)
import Nixon.Project (Project, ProjectType (..), project_path)
import qualified Nixon.Project as P
import Nixon.Select (Selection (..), Selector)
import qualified Nixon.Select as Select
import Nixon.Types
  ( Config (commands, project_dirs, project_types),
    Env (backend, config),
    Nixon,
    NixonError (EmptyError, NixonError),
    runNixon,
  )
import Nixon.Utils (implode_home)
import System.Console.Haskeline (defaultSettings, getInputLineWithInitial, runInputT)
import System.Environment (withArgs)
import Turtle
  ( ExitCode (ExitFailure),
    FilePath,
    MonadIO (..),
    Text,
    cd,
    d,
    exit,
    format,
    fp,
    lineToText,
    printf,
    pwd,
    s,
    select,
    stream,
    w,
    (%),
  )
import qualified Turtle.Bytes as BS
import Prelude hiding (FilePath, fail, log)

-- | List projects, filtering if a filter is specified.
list_projects :: [Project] -> Maybe Text -> Nixon ()
list_projects projects query = do
  let fmt_line = fmap (Select.Identity . format fp)
  paths <- liftIO $ fmt_line <$> traverse (implode_home . project_path) projects
  let fzf_opts = fzfFilter $ fromMaybe "" query
  liftIO (fzf fzf_opts (Turtle.select paths)) >>= \case
    Selection _ matching -> liftIO $ T.putStr matching
    _ -> log_error "No projects."

fail :: MonadIO m => NixonError -> m a
fail err = liftIO (throwIO err)

-- | Attempt to parse a local JSON
with_local_config :: FilePath -> Nixon a -> Nixon a
with_local_config filepath action = do
  liftIO (Config.find_local_config filepath) >>= \case
    Nothing -> action
    Just cfg -> local (\env -> env {config = config env <> cfg}) action

list_commands :: Project -> Nixon [Command]
list_commands project = filter filter_cmd . commands . config <$> ask
  where
    ptypes = map project_id $ P.project_types project
    filter_cmd cmd =
      let ctypes = cmdProjectTypes cmd
       in null ctypes || not (null $ intersect ptypes ctypes)

-- | Find and handle a command in a project.
findAndHandleCmd :: Project -> RunOpts -> Nixon ()
findAndHandleCmd project opts = with_local_config (project_path project) $ do
  find_command <- Backend.commandSelector <$> getBackend
  cmds <- list_commands project
  cmd <- liftIO $ find_command project opts cmds
  handleCmd project cmd opts

-- | Find and run a command in a project.
handleCmd :: Project -> Selection Command -> RunOpts -> Nixon ()
handleCmd project cmd opts = do
  selector <- Backend.selector <$> getBackend
  case cmd of
    EmptySelection -> fail $ EmptyError "No command selected."
    CanceledSelection -> fail $ EmptyError "Command selection canceled."
    Selection selectionType cmd' ->
      if Opts.run_select opts
        then do
          resolved <- resolve_cmd project selector cmd' Select.defaults
          printf (s % "\n") resolved
        else do
          case selectionType of
            Select.Default -> runCmd project cmd' (Opts.run_args opts)
            Select.Edit -> editCmd project cmd' (Opts.run_args opts)
            Select.Show -> showCmd cmd'
            Select.Visit -> visitCmd cmd'

-- | Actually run a command
runCmd :: Project -> Command -> [Text] -> Nixon ()
runCmd project cmd args = do
  selector <- Backend.selector <$> getBackend
  let project_selector select_opts shell' =
        cd (project_path project)
          >> selector (select_opts <> Select.title (show_command cmd)) shell'
  env' <- resolve_env project project_selector cmd args
  evaluate cmd (Just $ project_path project) env'

-- | Edit the command source before execution
editCmd :: Project -> Command -> [Text] -> Nixon ()
editCmd project cmd args = do
  edited <- editSelection (T.strip $ Cmd.cmdSource cmd)
  case edited of
    Nothing -> fail $ EmptyError "Empty command."
    Just source -> runCmd project (cmd {Cmd.cmdSource = source}) args

-- | Print the command
showCmd :: Command -> Nixon ()
showCmd = liftIO . T.putStrLn . Cmd.cmdSource

-- | "Visit" the command where it's defined
visitCmd :: Command -> Nixon ()
visitCmd cmd =
  case Cmd.cmdLocation cmd of
    Nothing -> fail $ NixonError "Unable to find command location."
    Just loc -> do
      let args =
            [ format fp $ Cmd.cmdFilePath loc,
              format ("+" % d) $ Cmd.cmdLineNr loc
            ]
      -- TODO: Replace with $EDITOR
      run ("vim" :| args) Nothing []

-- | Resolve all command environment variable placeholders.
resolve_env :: Project -> Selector Nixon -> Command -> [Text] -> Nixon Nixon.Process.Env
resolve_env project selector cmd args = do
  vars <- mapM resolve_each $ zip (cmdEnv cmd) (map Select.search args <> repeat Select.defaults)
  pure $ nixon_envs ++ vars
  where
    nixon_envs = [("nixon_project_path", format fp $ project_path project)]
    resolve_each ((name, Env cmd'), select_opts) =
      (name,)
        <$> ( assert_command cmd' >>= \c -> resolve_cmd project selector c select_opts
            )
    assert_command cmd_name = do
      cmd' <- find ((==) cmd_name . cmdName) . commands . config <$> ask
      maybe (error $ "Invalid argument: " <> T.unpack cmd_name) pure cmd'

-- | Resolve command to selectable output.
resolve_cmd :: Project -> Selector Nixon -> Command -> Select.SelectorOpts -> Nixon Text
resolve_cmd project selector cmd select_opts = do
  env' <- resolve_env project selector cmd []
  let path' = Just $ project_path project
  linesEval <- getEvaluator (run_with_output stream) cmd path' env'
  jsonEval <- getEvaluator (run_with_output BS.stream) cmd path' env'
  selection <- selector select_opts $ do
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

getBackend :: Nixon Backend
getBackend = do
  env <- ask
  let cfg = config env
  pure $ case backend env of
    Fzf -> fzfBackend cfg
    Rofi -> rofiBackend cfg

-- | Use readline to manipulate/change a fzf selection
editSelection :: (MonadIO m, MonadMask m) => Text -> m (Maybe Text)
editSelection selection = runInputT defaultSettings $ do
  line <- getInputLineWithInitial "> " (T.unpack selection, "")
  pure $ case line of
    Just "" -> Nothing
    line' -> T.pack <$> line'

-- | Find/filter out a project and perform an action.
project_action :: [Project] -> ProjectOpts -> Nixon ()
project_action projects opts
  | Opts.proj_list opts = list_projects projects (Opts.proj_project opts)
  | otherwise = do
    ptypes <- project_types . config <$> ask
    projectSelector <- Backend.projectSelector <$> getBackend

    let find_project' (Just ".") = do
          inProject <- P.find_in_project ptypes =<< pwd
          case inProject of
            Nothing -> projectSelector Nothing projects
            Just project -> pure $ Selection Select.Default project
        find_project' query = projectSelector query projects

    selection <- liftIO $ find_project' (Opts.proj_project opts)
    case selection of
      EmptySelection -> liftIO (throwIO $ EmptyError "No command selected.")
      CanceledSelection -> liftIO (throwIO $ EmptyError "Command selection canceled.")
      Selection _selectionType project ->
        if Opts.proj_select opts
          then liftIO $ printf (fp % "\n") (project_path project)
          else
            let opts' = RunOpts (Opts.proj_command opts) (Opts.proj_args opts) (Opts.proj_list opts) (Opts.proj_select opts)
             in findAndHandleCmd project opts'

-- | Run a command from current directory
run_action :: RunOpts -> Nixon ()
run_action opts = do
  ptypes <- project_types . config <$> ask
  project <- liftIO (P.find_in_project_or_default ptypes =<< pwd)
  if Opts.run_list opts
    then list_commands project >>= liftIO . mapM_ (T.putStrLn . show_command_with_description)
    else findAndHandleCmd project opts

die :: (Show a, MonadIO m) => a -> m b
die err = liftIO $ log_error (format w err) >> exit (ExitFailure 1)

-- If switching to a project takes a long time it would be nice to see a window
-- showing the progress of starting the environment.
nixon_with_config :: MonadIO m => Config.Config -> m ()
nixon_with_config user_config = liftIO $ do
  (sub_cmd, cfg) <- either die pure =<< Opts.parse_args (nixon_completer user_config)
  err <- try $
    runNixon (user_config <> cfg) $ case sub_cmd of
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
    Left (NixonError msg) -> die msg
    Right _ -> pure ()

nixon_completer :: MonadIO m => Config.Config -> CompletionType -> [String] -> m [String]
nixon_completer user_config comp_type args = do
  let parse_args = Opts.parse_args $ nixon_completer user_config
  (_, cfg) <- liftIO $ either die pure =<< withArgs args parse_args
  liftIO $
    runNixon (user_config <> cfg) $ do
      cfg' <- config <$> ask
      let ptypes = project_types cfg'
          srcs = project_dirs cfg'
      projects <- P.sort_projects <$> liftIO (P.find_projects 1 ptypes srcs)
      case comp_type of
        Opts.Project -> pure $ map (T.unpack . format fp . P.project_name) projects
        Opts.Run -> do
          project <- case args of
            ("project" : p : _) -> do
              current <- P.from_path <$> pwd
              let p' = find ((==) p . T.unpack . format fp . P.project_name) projects
              pure $ fromMaybe current p'
            _ -> liftIO $ P.find_in_project_or_default ptypes =<< pwd
          commands <- with_local_config (project_path project) $ list_commands project
          pure $ map (T.unpack . cmdName) commands

-- | Nixon with default configuration
nixon :: MonadIO m => m ()
nixon = nixon_with_config Config.defaultConfig
