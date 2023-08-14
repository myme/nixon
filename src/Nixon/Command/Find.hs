-- | Functions for command discovery
module Nixon.Command.Find where

import Control.Monad.Trans.Reader (ask, local)
import Data.Function (on)
import qualified Data.List as List
import Nixon.Backend (Backend)
import qualified Nixon.Backend as Backend
import qualified Nixon.Backend.Fzf as Fzf
import qualified Nixon.Backend.Rofi as Rofi
import Nixon.Command (Command (cmdName, cmdSource))
import qualified Nixon.Command as Cmd
import qualified Nixon.Config as Config
import Nixon.Config.Options (RunOpts)
import qualified Nixon.Config.Options as Opts
import Nixon.Config.Types (Config (project_types), commands)
import qualified Nixon.Config.Types as Config
import Nixon.Prelude
import Nixon.Process (HasProc)
import Nixon.Project (Project, ProjectType (..), project_path)
import qualified Nixon.Project as P
import Nixon.Select (Selection (..))
import qualified Nixon.Select as Select
import Nixon.Types (Nixon, config)
import qualified Nixon.Types as Env
import Nixon.Utils (shell_to_list)
import Turtle
  ( Permissions (_executable),
    basename,
    format,
    fp,
    getmod,
    lsif,
    pwd,
    select,
    (%),
    (</>),
  )

findProjectCommands :: Project -> Nixon [Command]
findProjectCommands project = do
  markdownCmds <- filter filter_cmd . commands . config <$> ask
  binCmds <- findBinCommands project
  let commands = markdownCmds <> binCmds
      sortedCmds = List.sortBy (compare `on` cmdName) commands
  pure sortedCmds
  where
    ptypes = map project_id $ P.project_types project
    filter_cmd cmd =
      let ctypes = Cmd.cmdProjectTypes cmd
       in null ctypes || not (null $ List.intersect ptypes ctypes)

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
          cmdSource = format (fp % " \"$@\"") path
        }
  where
    isExecutable = fmap _executable . getmod

type CommandHandler = Project -> Selection Command -> RunOpts -> Nixon ()

-- | Find and handle a command in a project.
findAndHandleCmd :: CommandHandler -> Project -> RunOpts -> Nixon ()
findAndHandleCmd handleCmd project opts = withLocalConfig (project_path project) $ do
  find_command <- Backend.commandSelector <$> getBackend
  cmds <- filter (not . Cmd.cmdIsHidden) <$> findProjectCommands project
  cmd <- liftIO $ find_command project (Opts.runCommand opts) cmds
  handleCmd project cmd opts

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

getBackend :: (HasProc m, MonadIO m) => Nixon (Backend m)
getBackend = do
  env <- ask
  let cfg = config env
  pure $ case Env.backend env of
    Config.Fzf -> Fzf.fzfBackend cfg
    Config.Rofi -> Rofi.rofiBackend cfg

-- | Attempt to parse a local JSON
withLocalConfig :: FilePath -> Nixon a -> Nixon a
withLocalConfig filepath action = do
  liftIO (Config.findLocalConfig filepath) >>= \case
    Nothing -> action
    Just cfg -> local (\env -> env {config = config env <> cfg}) action
