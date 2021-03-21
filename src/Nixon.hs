module Nixon
  ( nixon
  , nixon_with_config
  ) where

import           Control.Arrow (second)
import           Control.Exception
import           Control.Monad.Trans.Maybe
import           Control.Monad.Trans.Reader
import           Data.Aeson
import           Data.Foldable (find)
import           Data.List (intersect)
import           Data.Maybe (fromMaybe, catMaybes)
import qualified Data.Text as T
import           Data.Text.Encoding (encodeUtf8)
import qualified Data.Text.IO as T
import           Nixon.Command
import qualified Nixon.Config as Config
import           Nixon.Config.Options (Backend(..), ProjectOpts, SubCommand(..))
import qualified Nixon.Config.Options as Options
import           Nixon.Config.Types (ignore_case)
import qualified Nixon.Config.Types as Config
import           Nixon.Direnv
import           Nixon.Fzf
import           Nixon.Logging
import           Nixon.Nix
import           Nixon.Process (Env, spawn, run)
import qualified Nixon.Project as P
import           Nixon.Project hiding (project_types)
import           Nixon.Rofi
import           Nixon.Select (Selection(..), Selector)
import qualified Nixon.Select as Select
import           Nixon.Types hiding (Env)
import           Nixon.Utils
import           Prelude hiding (FilePath, log)
import qualified System.IO as IO
import           Turtle hiding (decimal, die, env, err, find, output, shell, text, x)

-- | List projects, filtering if a filter is specified.
list :: [Project] -> Maybe Text -> Nixon ()
list projects query = do
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

-- | Maybe wrap a command in direnv/nix.
maybe_wrap_cmd :: Project -> Command -> Nixon Command
maybe_wrap_cmd project cmd = fmap (fromMaybe cmd) $ runMaybeT
   $  MaybeT (direnv_cmd cmd (project_path project))
  <|> MaybeT (nix_cmd cmd (project_path project))

-- | Attempt to parse a local JSON
with_local_config :: Project -> Nixon () -> Nixon ()
with_local_config project action = do
  liftIO (Config.find_local_config (project_path project)) >>= \case
    Nothing -> action
    Just cfg -> local (\env -> env { config = config env <> cfg }) action

-- | Find and run a command in a project.
-- TODO: Print command before running it (add -q|--quiet)
run_cmd :: CommandSelector
        -> Project
        -> ProjectOpts
        -> Selector
        -> Nixon ()
run_cmd select_command project opts selector = with_local_config project $ do
  let ptypes = map project_id $ P.project_types project
      filter_cmd cmd = let ctypes = cmdProjectTypes cmd
                       in null ctypes || not (null $ intersect ptypes ctypes)
      project_selector = \shell' -> cd (project_path project) >> selector shell'
  cmds <- filter filter_cmd . commands . config <$> ask
  cmd <- liftIO $ fail_empty "No command selected." $ select_command project opts cmds
  (cmd', env') <- maybe_wrap_cmd project cmd >>= resolve_command project project_selector
  -- TODO: Always edit command before executing?
  log_info (format ("Running command "%w) cmd')
  project_exec cmd' (cmdIsBg cmd) project env'

project_exec :: Text -> Bool -> Project -> Env -> Nixon ()
project_exec cmd is_bg project env' = do
  isTTY <- (&&) <$> (not . Config.isGuiBackend . backend <$> ask) <*> liftIO (IO.hIsTerminalDevice IO.stdin)
  if isTTY
    then run [cmd] (Just $ project_path project) env'
    else do
      shell' <- fromMaybe "bash" <$> need "SHELL"
      let path' = Just $ project_path project
      if is_bg
        then spawn (shell' : ["-c", quote cmd]) path' env'
        else do
          let end = "; echo -e " <> quote "\n[Press Return to exit]" <> "; read"
              cmd' = shell' : ["-c", quote (cmd <> end)]
          term <- fmap (fromMaybe "x-terminal-emulator") $ runMaybeT
             $  MaybeT (terminal . config <$> ask)
            <|> MaybeT (need "TERMINAL")
          spawn (term : "-e" : cmd') path' env'

resolve_command :: Project -> Selector -> Command -> Nixon (Text, Env)
resolve_command project selector cmd = (,) (cmdSource cmd) <$> resolve_args (cmdEnv cmd)
  where
    resolve_args = fmap concat . mapM resolve_arg
    resolve_arg (name, Env p) = find ((==) p . cmdName) . commands . config <$> ask >>= \case
      Nothing -> error $ "Invalid argument: " <> T.unpack p
      Just arg' -> do
        (resolved, _) <- maybe_wrap_cmd project arg' >>= resolve_command project selector
        selection <- liftIO $ selector $ do
          case cmdOutput arg' of
            Lines -> do
              candidate <- inshell resolved empty
              pure $ Select.Identity (lineToText candidate)
            JSON -> do
              (_, output) <- second encodeUtf8 <$> shellStrict resolved empty
              case eitherDecodeStrict output :: Either String [Select.Candidate] of
                Left err -> error err
                Right candidates -> select candidates
        case selection of
          Selection _ result -> pure [(name, result)]
          _ -> error "Argument expansion aborted"

type ProjectSelector = Maybe Text -> [Project] -> IO (Maybe Project)
type CommandSelector = Project -> ProjectOpts -> [Command] -> IO (Maybe Command)

get_selectors :: Nixon ([ProjectType], ProjectSelector, CommandSelector, Selector)
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
  | Options.list opts = Nixon.list projects (Options.project opts)
  | otherwise = do
      (ptypes, find_project, find_command, selector) <- get_selectors

      -- TODO: Generalize rofi/fzf_projects and move this to Nixon.Project using `select`
      let find_project' (Just ".") = runMaybeT
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
  (ptypes, _project, find_command, selector) <- get_selectors
  project <- liftIO (find_in_project_or_default ptypes =<< pwd)
  run_cmd find_command project opts selector

-- TODO: Launch terminal with nix-shell output if taking a long time.
-- TODO: Allow changing default command
-- TODO: Project local commands (project/path/.nixon)
-- TODO: Pingbot integration?
-- If switching to a project takes a long time it would be nice to see a window
-- showing the progress of starting the environment.
nixon_with_config :: MonadIO m => Config.Config -> m ()
nixon_with_config user_config = liftIO $ do
  (sub_cmd, cfg) <- either die pure =<< Options.parse_args
  err <- try $ runNixon (user_config <> cfg) $ case sub_cmd of
    ProjectCommand project_opts -> do
      log_info "Running <project> command"
      cfg' <- config <$> ask
      let ptypes = project_types cfg'
          srcs = project_dirs cfg'
      projects <- sort_projects <$> liftIO (find_projects 1 ptypes srcs)
      project_action projects project_opts
    RunCommand run_opts -> do
      log_info "Running <run> command"
      run_action run_opts
  case err of
    Left (EmptyError msg) -> die msg
    Right _ -> pure ()
  where die err = liftIO $ log_error (format w err) >> exit (ExitFailure 1)

-- | Nixon with default configuration
nixon :: MonadIO m => m ()
nixon = nixon_with_config Config.defaultConfig
