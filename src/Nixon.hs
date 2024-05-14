{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedRecordDot #-}

module Nixon
  ( nixon,
    nixonWithConfig,
  )
where

import Control.Exception (throwIO, try)
import Control.Monad.Catch (MonadMask)
import Control.Monad.Trans.Reader (ask)
import Data.Foldable (find)
import Data.Functor (void)
import Data.List.NonEmpty (NonEmpty ((:|)))
import Data.Maybe (fromMaybe)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Data.Traversable (for)
import qualified Nixon.Backend as Backend
import Nixon.Backend.Fzf
  ( fzf,
    fzfFilter,
  )
import Nixon.Command (Command (..), show_command_with_description)
import qualified Nixon.Command as Cmd
import Nixon.Command.Find (findAndHandleCmd, findCmd, findProject, findProjectCommands, getBackend, withLocalConfig)
import qualified Nixon.Command.Run as Cmd
import Nixon.Config.Options (CompletionType, EditOpts (..), EvalOpts (..), EvalSource (..), GCOpts (..), NewOpts (..), ProjectOpts (..), RunOpts (..), SubCommand (..))
import qualified Nixon.Config.Options as Opts
import qualified Nixon.Config.Types as Config
import Nixon.Evaluator (garbageCollect)
import Nixon.Language (Language (..), fromFilePath)
import Nixon.Logging (log_error, log_info)
import Nixon.Prelude
import Nixon.Process (run)
import Nixon.Project (Project, project_path, inspectProjects)
import qualified Nixon.Project as P
import Nixon.Select (Candidate (..), Selection (..), SelectionType (..))
import qualified Nixon.Select as Select
import Nixon.Types
  ( Config (project_dirs, project_types),
    Env (config),
    Nixon,
    NixonError (EmptyError, NixonError),
    runNixon,
  )
import Nixon.Utils (confirm, fromPath, implode_home, openEditor)
import System.Console.Haskeline (defaultSettings, getInputLineWithInitial, runInputT)
import System.Environment (withArgs)
import Turtle (Alternative (empty), ExitCode (ExitFailure), exit, format, fp, mktempfile, printf, pwd, s, select, sh, w, (%))

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

fail :: (MonadIO m) => NixonError -> m a
fail err = liftIO (throwIO err)

-- | Find and run a command in a project.
handleCmd :: Project -> Selection Command -> RunOpts -> Nixon ()
handleCmd project cmd opts = do
  selector <- Backend.selector <$> getBackend
  case cmd of
    EmptySelection -> fail $ EmptyError "No command selected."
    CanceledSelection -> fail $ EmptyError "Command selection canceled."
    Selection _ [] -> fail $ EmptyError "No command selected."
    Selection selectionType [cmd']
      | opts.runInsert -> liftIO $ T.putStrLn cmd'.cmdSource
      | opts.runSelect -> do
          let selectOpts = Select.defaults {Select.selector_multiple = Just True}
          resolved <- Cmd.resolveCmd project selector cmd' selectOpts
          printf (s % "\n") (T.unlines resolved)
      | otherwise -> do
          case selectionType of
            Select.Default -> Cmd.runCmd selector project cmd' (Opts.runArgs opts)
            Select.Edit -> editCmd project cmd' (Opts.runArgs opts)
            Select.Show -> showCmd cmd'
            Select.Visit -> visitCmd cmd'
    Selection _ _ -> fail $ NixonError "Multiple commands selected."

-- | Edit the command source before execution
editCmd :: Project -> Command -> [Text] -> Nixon ()
editCmd project cmd args = do
  selector <- Backend.selector <$> getBackend
  edited <- editSelection (T.strip $ Cmd.cmdSource cmd)
  case edited of
    Nothing -> fail $ EmptyError "Empty command."
    Just source -> Cmd.runCmd selector project (cmd {Cmd.cmdSource = source}) args

-- | Print the command
showCmd :: Command -> Nixon ()
showCmd = liftIO . T.putStrLn . Cmd.cmdSource

-- | "Visit" the command where it's defined
visitCmd :: Command -> Nixon ()
visitCmd cmd =
  case Cmd.cmdLocation cmd of
    Nothing -> fail $ NixonError "Unable to find command location."
    Just loc -> openEditor loc.cmdFilePath (Just loc.cmdStartLine)

-- | Use readline to manipulate/change a fzf selection
editSelection :: (MonadIO m, MonadMask m) => Text -> m (Maybe Text)
editSelection selection = runInputT defaultSettings $ do
  line <- getInputLineWithInitial "> " (T.unpack selection, "")
  pure $ case line of
    Just "" -> Nothing
    line' -> T.pack <$> line'

-- | Jump to command expression in $EDITOR.
editAction :: EditOpts -> Nixon ()
editAction opts = do
  log_info "Running <edit> command"
  cmd <- findCmd "Edit command" opts.editCommand
  case cmd of
    EmptySelection -> die ("No command selected." :: String)
    CanceledSelection -> die ("Command selection canceled." :: String)
    Selection _ [cmd'] -> visitCmd cmd'
    _ -> die ("Multiple commands selected." :: String)

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
       in liftIO ((,) <$> T.readFile (fromPath filePath) <*> pure lang')

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
            runInsert = False,
            runList = False,
            runSelect = False
          }

  handleCmd project (Selection Default [cmd']) runOpts

-- | Garbage collect cached scripts
gcAction :: GCOpts -> Nixon ()
gcAction gcOpts = do
  files <- garbageCollect (gcDryRun gcOpts)
  void . liftIO . for files $ T.putStrLn

-- | Create a new command and insert into project file
newAction :: NewOpts -> Nixon ()
newAction opts =
  findCmd "Insert after" Nothing >>= \case
    EmptySelection -> die ("No command selected." :: String)
    CanceledSelection -> die ("Command selection canceled." :: String)
    Selection _ [cmd'] -> do
      case cmd'.cmdLocation of
        Nothing -> die ("Unable to find command location." :: String)
        Just loc -> sh $ do
          tmpPath <- mktempfile "/tmp" "nixon.md"
          liftIO $ do
            contents <- T.readFile loc.cmdFilePath
            let (before, after) = splitAt loc.cmdEndLine (T.lines contents)
                template =
                  T.unlines
                    [ format (s % " `" % s % "`") (T.replicate loc.cmdLevel "#") opts.name,
                      "",
                      opts.description,
                      "",
                      format ("```" % w) opts.language,
                      opts.source,
                      "```"
                    ]
            T.writeFile tmpPath $ T.unlines $ before <> [template] <> after
          -- Edit the command
          openEditor tmpPath (Just $ loc.cmdEndLine + 1)
          -- Diff the end result
          run ("diff" :| (T.pack <$> ["-u", "--color=always", loc.cmdFilePath, tmpPath])) Nothing [] empty
          -- Prompt to confirm updating the nixon.md file
          answer <- confirm $ format ("Update " % fp % "? [y/N] ") loc.cmdFilePath
          -- Update the nixon.md file
          liftIO
            $ if answer
              then do
                log_info $ format ("Updating " % fp % "…") loc.cmdFilePath
                T.writeFile loc.cmdFilePath =<< T.readFile tmpPath
              else log_info "Update canceled."
    _ -> die ("Multiple commands selected." :: String)

-- | Find/filter out a project and perform an action.
projectAction :: [Project] -> ProjectOpts -> Nixon ()
projectAction projects opts
  | opts.projList = listProjects projects (Opts.projProject opts)
  | otherwise = do
      let multiple = opts.projSelect || opts.projInspect
          selectOpts = Select.defaults {Select.selector_multiple = Just multiple}

      selection <- findProject projects selectOpts (Opts.projProject opts)
      case selection of
        EmptySelection -> liftIO (throwIO $ EmptyError "No project selected.")
        CanceledSelection -> liftIO (throwIO $ EmptyError "Project selection canceled.")
        Selection selectionType ps ->
          if
            | opts.projSelect -> void . liftIO . for ps $ printf (fp % "\n") . project_path
            | opts.projInspect || selectionType == Select.Show -> inspectProjects ps
            | otherwise -> case ps of
                [project] ->
                  let opts' =
                        RunOpts
                          opts.projCommand
                          opts.projArgs
                          opts.projInsert
                          opts.projList
                          opts.projSelect
                   in findAndHandleCmd handleCmd project opts'
                _ -> liftIO (throwIO $ NixonError "Multiple projects selected.")

-- | Run a command from current directory
runAction :: RunOpts -> Nixon ()
runAction opts = do
  ptypes <- project_types . config <$> ask
  project <- liftIO (P.find_in_project_or_default ptypes =<< pwd)
  if Opts.runList opts
    then listProjectCommands project (Opts.runCommand opts)
    else findAndHandleCmd handleCmd project opts

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
nixonWithConfig :: (MonadIO m) => Config.Config -> m ()
nixonWithConfig userConfig = liftIO $ do
  (sub_cmd, cfg) <- either die pure =<< Opts.parseArgs (nixonCompleter userConfig)

  err <- try
    $ runNixon (userConfig <> cfg)
    $ do
      projects <- getSortedProjects
      case sub_cmd of
        EditCommand editOpts -> do
          log_info "Running <edit> command"
          editAction editOpts
        EvalCommand evalOpts -> do
          log_info "Running <eval> command"
          evalAction projects evalOpts
        GCCommand gcOpts -> do
          log_info "Running <gc> command"
          gcAction gcOpts
        NewCommand newOpts -> do
          log_info "Running <new> command"
          newAction newOpts
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

nixonCompleter :: (MonadIO m) => Config.Config -> CompletionType -> [String] -> m [String]
nixonCompleter userConfig compType args = do
  let parse_args = Opts.parseArgs $ nixonCompleter userConfig
  (_, cfg) <- liftIO $ either die pure =<< withArgs args parse_args
  liftIO
    $ runNixon (userConfig <> cfg)
    $ do
      projects <- getSortedProjects
      case compType of
        Opts.Edit -> do
          project <- case args of
            ("project" : p : _) -> do
              current <- P.from_path <$> pwd
              let p' = find ((==) p . T.unpack . format fp . P.projectName) projects
              pure $ fromMaybe current p'
            _ -> do
              ptypes <- project_types . config <$> ask
              liftIO $ P.find_in_project_or_default ptypes =<< pwd
          commands <- withLocalConfig (project_path project) $ findProjectCommands project
          pure $ map (T.unpack . cmdName) commands
        Opts.Eval -> pure []
        Opts.Project -> pure $ map (T.unpack . format fp . P.projectName) projects
        Opts.Run -> do
          project <- case args of
            ("project" : p : _) -> do
              current <- P.from_path <$> pwd
              let p' = find ((==) p . T.unpack . format fp . P.projectName) projects
              pure $ fromMaybe current p'
            _ -> do
              ptypes <- project_types . config <$> ask
              liftIO $ P.find_in_project_or_default ptypes =<< pwd
          commands <- withLocalConfig (project_path project) $ findProjectCommands project
          pure $ map (T.unpack . cmdName) commands

-- | Nixon with default configuration
nixon :: (MonadIO m) => m ()
nixon = nixonWithConfig Config.defaultConfig
