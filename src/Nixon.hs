module Nixon
  ( nixon,
    nixonWithConfig,
  )
where

import Control.Exception (throwIO, try)
import Control.Monad.Catch (MonadMask)
import Control.Monad.Trans.Maybe (MaybeT (..))
import Control.Monad.Trans.Reader (ask, local)
import Data.Foldable (find)
import Data.Function (on)
import Data.Functor (void)
import Data.List (intersect, sortBy)
import Data.List.NonEmpty (NonEmpty ((:|)))
import Data.Maybe (fromMaybe)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Data.Traversable (for)
import Nixon.Backend (Backend)
import qualified Nixon.Backend as Backend
import Nixon.Backend.Fzf
  ( fzf,
    fzfBackend,
    fzfFilter,
  )
import Nixon.Backend.Rofi (rofiBackend)
import Nixon.Command (Command (..), show_command_with_description)
import qualified Nixon.Command as Cmd
import qualified Nixon.Command.Run as Cmd
import qualified Nixon.Config as Config
import Nixon.Config.Options (BackendType (..), CompletionType, EvalOpts (..), EvalSource (..), GCOpts (..), ProjectOpts (..), RunOpts (..), SubCommand (..))
import qualified Nixon.Config.Options as Opts
import qualified Nixon.Config.Types as Config
import Nixon.Evaluator (garbageCollect)
import Nixon.Language (Language (..), fromFilePath)
import Nixon.Logging (log_error, log_info)
import Nixon.Prelude
import Nixon.Process (run)
import Nixon.Project (Project, ProjectType (..), project_path)
import qualified Nixon.Project as P
import Nixon.Select (Candidate (..), Selection (..), SelectionType (..))
import qualified Nixon.Select as Select
import Nixon.Types
  ( Config (commands, project_dirs, project_types),
    Env (backend, config),
    Nixon,
    NixonError (EmptyError, NixonError),
    runNixon,
  )
import Nixon.Utils (implode_home, shell_to_list)
import System.Console.Haskeline (defaultSettings, getInputLineWithInitial, runInputT)
import System.Environment (withArgs)
import Turtle
  ( Alternative (empty),
    ExitCode (ExitFailure),
    basename,
    d,
    exit,
    format,
    fp,
    getmod,
    lsif,
    need,
    printf,
    pwd,
    readTextFile,
    s,
    select,
    w,
    (%),
    (</>),
    _executable,
  )

-- | List projects, filtering if a query is specified.
listProjects :: [Project] -> Maybe Text -> Nixon ()
listProjects projects query = do
  let fmt_line = fmap (Select.Identity . format fp)
  paths <- liftIO $ fmt_line <$> traverse (implode_home . project_path) projects
  let fzf_opts = fzfFilter $ fromMaybe "" query
  liftIO (fzf fzf_opts (Turtle.select paths)) >>= \case
    Selection _ matching -> liftIO $ T.putStr (T.unlines matching)
    _ -> log_error "No projects."

-- | List commands for a project, filtering if a query is specified
listProjectCommands :: Project -> Maybe Text -> Nixon ()
listProjectCommands project query = do
  commands <- map show_command_with_description <$> findProjectCommands project
  let fzfOpts = fzfFilter $ fromMaybe "" query
  selection <- fzf fzfOpts (Turtle.select $ Identity <$> commands)
  case selection of
    Selection _ matching -> liftIO $ T.putStr (T.unlines matching)
    _ -> log_error "No commands."

fail :: MonadIO m => NixonError -> m a
fail err = liftIO (throwIO err)

-- | Attempt to parse a local JSON
withLocalConfig :: FilePath -> Nixon a -> Nixon a
withLocalConfig filepath action = do
  liftIO (Config.findLocalConfig filepath) >>= \case
    Nothing -> action
    Just cfg -> local (\env -> env {config = config env <> cfg}) action

findProjectCommands :: Project -> Nixon [Command]
findProjectCommands project = do
  markdownCmds <- filter filter_cmd . commands . config <$> ask
  binCmds <- findBinCommands project
  let commands = markdownCmds <> binCmds
      sortedCmds = sortBy (compare `on` cmdName) commands
  pure sortedCmds
  where
    ptypes = map project_id $ P.project_types project
    filter_cmd cmd =
      let ctypes = cmdProjectTypes cmd
       in null ctypes || not (null $ intersect ptypes ctypes)

-- | Find executables within a "bin" directory in the project
findBinCommands :: Project -> Nixon [Command]
findBinCommands project = do
  binDirs <- Config.bin_dirs . config <$> ask
  shell_to_list $ do
    dir <- select binDirs
    let binPath = project_path project </> dir
    path <- lsif isExecutable binPath
    pure
      Cmd.empty
        { cmdName = format fp (basename path),
          cmdSource = format fp path
        }
  where
    isExecutable = fmap _executable . getmod

-- | Find and handle a command in a project.
findAndHandleCmd :: Project -> RunOpts -> Nixon ()
findAndHandleCmd project opts = withLocalConfig (project_path project) $ do
  find_command <- Backend.commandSelector <$> getBackend
  cmds <- filter (not . Cmd.cmdIsHidden) <$> findProjectCommands project
  cmd <- liftIO $ find_command project (Opts.runCommand opts) cmds
  handleCmd (project_path project) cmd opts

-- | Find and run a command in a project.
handleCmd :: FilePath -> Selection Command -> RunOpts -> Nixon ()
handleCmd path cmd opts = do
  selector <- Backend.selector <$> getBackend
  case cmd of
    EmptySelection -> fail $ EmptyError "No command selected."
    CanceledSelection -> fail $ EmptyError "Command selection canceled."
    Selection _ [] -> fail $ EmptyError "No command selected."
    Selection selectionType [cmd'] ->
      if Opts.runSelect opts
        then do
          let selectOpts = Select.defaults {Select.selector_multiple = Just True}
          resolved <- Cmd.resolveCmd path selector cmd' selectOpts
          printf (s % "\n") (T.unlines resolved)
        else do
          case selectionType of
            Select.Default -> Cmd.runCmd selector path cmd' (Opts.runArgs opts)
            Select.Edit -> editCmd path cmd' (Opts.runArgs opts)
            Select.Show -> showCmd cmd'
            Select.Visit -> visitCmd cmd'
    Selection _ _ -> fail $ NixonError "Multiple commands selected."

-- | Edit the command source before execution
editCmd :: FilePath -> Command -> [Text] -> Nixon ()
editCmd path cmd args = do
  selector <- Backend.selector <$> getBackend
  edited <- editSelection (T.strip $ Cmd.cmdSource cmd)
  case edited of
    Nothing -> fail $ EmptyError "Empty command."
    Just source -> Cmd.runCmd selector path (cmd {Cmd.cmdSource = source}) args

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
            [ format ("+" % d) $ Cmd.cmdLineNr loc,
              format fp $ Cmd.cmdFilePath loc
            ]
      editor <-
        fromMaybe "nano"
          <$> runMaybeT
            ( MaybeT (need "VISUAL") <|> MaybeT (need "EDITOR")
            )
      run (editor :| args) Nothing [] empty

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

-- | Evaluate a command expression.
evalAction :: [Project] -> EvalOpts -> Nixon ()
evalAction projects (EvalOpts source placeholders lang projSelect) = do
  project <- do
    selection <- findProject projects Select.defaults (if projSelect then Nothing else Just ".")
    case selection of
      EmptySelection -> liftIO (throwIO $ EmptyError "No project selected.")
      CanceledSelection -> liftIO (throwIO $ EmptyError "Project selection canceled.")
      Selection _selectionType [project] -> pure project
      _ -> liftIO (throwIO $ NixonError "Multiple projects selected.")

  (source', lang') <- case source of
    EvalInline inline -> pure (inline, fromMaybe Bash lang)
    EvalFile filePath ->
      let lang' = fromMaybe (fromFilePath filePath) lang
       in liftIO ((,) <$> readTextFile filePath <*> pure lang')

  let cmd' =
        Cmd.empty
          { cmdSource = source',
            cmdPlaceholders = placeholders,
            cmdPwd = Just $ project_path project,
            cmdLang = lang'
          }
      runOpts =
        RunOpts
          { runCommand = Nothing,
            runArgs = [],
            runList = False,
            runSelect = False
          }

  handleCmd (project_path project) (Selection Default [cmd']) runOpts

-- | Garbage collect cached scripts
gcAction :: GCOpts -> Nixon ()
gcAction gcOpts = do
  files <- garbageCollect (gcDryRun gcOpts)
  void . liftIO . for files $ T.putStrLn

findProject :: [Project] -> Select.SelectorOpts -> Maybe Text -> Nixon (Selection Project)
findProject projects selectOpts query = do
  projectSelector <- Backend.projectSelector <$> getBackend
  case query of
    (Just ".") -> do
      ptypes <- project_types . config <$> ask
      inProject <- P.find_in_project ptypes =<< pwd
      case inProject of
        Nothing -> liftIO $ projectSelector selectOpts Nothing projects
        Just project -> pure $ Selection Select.Default [project]
    _ -> liftIO $ projectSelector selectOpts query projects

-- | Find/filter out a project and perform an action.
projectAction :: [Project] -> ProjectOpts -> Nixon ()
projectAction projects opts
  | Opts.projList opts = listProjects projects (Opts.projProject opts)
  | otherwise = do
      let selectOpts = Select.defaults {Select.selector_multiple = Just $ Opts.projSelect opts}

      selection <- findProject projects selectOpts (Opts.projProject opts)
      case selection of
        EmptySelection -> liftIO (throwIO $ EmptyError "No project selected.")
        CanceledSelection -> liftIO (throwIO $ EmptyError "Project selection canceled.")
        Selection _selectionType ps ->
          if Opts.projSelect opts
            then void . liftIO . for ps $ printf (fp % "\n") . project_path
            else case ps of
              [project] ->
                let opts' = RunOpts (Opts.projCommand opts) (Opts.projArgs opts) (Opts.projList opts) (Opts.projSelect opts)
                 in findAndHandleCmd project opts'
              _ -> liftIO (throwIO $ NixonError "Multiple projects selected.")

-- | Run a command from current directory
runAction :: RunOpts -> Nixon ()
runAction opts = do
  ptypes <- project_types . config <$> ask
  project <- liftIO (P.find_in_project_or_default ptypes =<< pwd)
  if Opts.runList opts
    then listProjectCommands project (Opts.runCommand opts)
    else findAndHandleCmd project opts

die :: (Show a, MonadIO m) => a -> m b
die err = liftIO $ log_error (format w err) >> exit (ExitFailure 1)

getSortedProjects :: Nixon [Project]
getSortedProjects = do
  cfg' <- config <$> ask
  let ptypes = project_types cfg'
      srcs = project_dirs cfg'
  P.sort_projects <$> liftIO (P.find_projects 1 ptypes srcs)

-- If switching to a project takes a long time it would be nice to see a window
-- showing the progress of starting the environment.
nixonWithConfig :: MonadIO m => Config.Config -> m ()
nixonWithConfig userConfig = liftIO $ do
  (sub_cmd, cfg) <- either die pure =<< Opts.parseArgs (nixonCompleter userConfig)

  err <- try $
    runNixon (userConfig <> cfg) $ do
      projects <- getSortedProjects
      case sub_cmd of
        EvalCommand evalOpts -> do
          log_info "Running <eval> command"
          evalAction projects evalOpts
        GCCommand gcOpts -> do
          log_info "Running <gc> command"
          gcAction gcOpts
        ProjectCommand projectOpts -> do
          log_info "Running <project> command"
          projectAction projects projectOpts
        RunCommand runOpts -> do
          log_info "Running <run> command"
          runAction runOpts

  case err of
    Left (EmptyError msg) -> die msg
    Left (NixonError msg) -> die msg
    Right _ -> pure ()

nixonCompleter :: MonadIO m => Config.Config -> CompletionType -> [String] -> m [String]
nixonCompleter userConfig compType args = do
  let parse_args = Opts.parseArgs $ nixonCompleter userConfig
  (_, cfg) <- liftIO $ either die pure =<< withArgs args parse_args
  liftIO $
    runNixon (userConfig <> cfg) $ do
      projects <- getSortedProjects
      case compType of
        Opts.Eval -> pure []
        Opts.Project -> pure $ map (T.unpack . format fp . P.project_name) projects
        Opts.Run -> do
          project <- case args of
            ("project" : p : _) -> do
              current <- P.from_path <$> pwd
              let p' = find ((==) p . T.unpack . format fp . P.project_name) projects
              pure $ fromMaybe current p'
            _ -> do
              ptypes <- project_types . config <$> ask
              liftIO $ P.find_in_project_or_default ptypes =<< pwd
          commands <- withLocalConfig (project_path project) $ findProjectCommands project
          pure $ map (T.unpack . cmdName) commands

-- | Nixon with default configuration
nixon :: MonadIO m => m ()
nixon = nixonWithConfig Config.defaultConfig
