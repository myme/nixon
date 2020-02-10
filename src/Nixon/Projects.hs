module Nixon.Projects
  ( Project (..)
  , Command
  , find_in_project
  , find_in_project_or_default
  , find_projects
  , find_projects_by_name
  , find_project_commands
  , implode_home
  , mkproject
  , parents
  , project_exec
  , project_path
  , resolve_command
  , sort_projects
  ) where

import           Control.Exception
import qualified Control.Foldl as Fold
import           Control.Monad (filterM)
import           Data.Function (on)
import           Data.List (sortBy)
import           Data.Maybe (fromMaybe)
import           Data.Text (isInfixOf)
import qualified Data.Text as T
import           Nixon.Process
import           Nixon.Projects.Types (command_gui, command_options)
import           Nixon.Projects.Types as Types
import           Nixon.Select (Select, Selection(..))
import qualified Nixon.Select as Select
import           Nixon.Types
import           Prelude hiding (FilePath)
import qualified System.IO as IO
import           System.Wordexp
import           Turtle hiding (f, find, sort, sortBy, text, toText)

mkproject :: FilePath -> Project
mkproject path' = Project (filename path') (parent path') []

-- | Replace the value of $HOME in a path with "~"
implode_home :: FilePath -> IO FilePath
implode_home path' = do
  home' <- home
  pure $ maybe path' ("~" </>) (stripPrefix (home' </> "") path')

parents :: FilePath -> [FilePath]
parents path'
  | path' == parent path' = [path']
  | otherwise = path' : parents (parent path')

-- | Find/filter out a project in which path is a subdirectory.
find_in_project :: [ProjectType] -> FilePath -> IO (Maybe Project)
find_in_project project_types path' = find_project project_types path' >>= \case
    Nothing -> if parent path' == root path'
      then pure Nothing
      else find_in_project project_types (parent path')
    project -> pure project

find_in_project_or_default :: [ProjectType] -> FilePath -> IO Project
find_in_project_or_default project_types path' = do
  types <- find_project_types path' project_types
  let current = (from_path path') { Types.project_types = types }
  fromMaybe current <$> find_in_project project_types path'

find_projects_by_name :: FilePath -> [ProjectType] -> [FilePath] -> IO [Project]
find_projects_by_name project project_types = fmap find_matching . find_projects 1 project_types
  where find_matching = filter ((project `isInfix`) . toText . project_name)
        isInfix p = isInfixOf (toText p)
        toText = format fp

find_project :: [ProjectType] -> FilePath -> IO (Maybe Project)
find_project project_types source_dir = do
  isdir <- testdir source_dir
  if not isdir
    then pure Nothing
    else do
      types <- find_project_types source_dir project_types
      if all (null . project_markers) types
        then pure Nothing
        else pure $ Just Project
          { Types.project_name = filename source_dir
          , Types.project_dir = parent source_dir
          , Types.project_types = types
          }

-- | Find projects from a list of source directories.
--
-- Filepath expansion is done on each source directory. For each source
-- directory not a project, look for subdirs being projects.
find_projects :: Integer -> [ProjectType] -> [FilePath] -> IO [Project]
find_projects max_depth project_types source_dirs
  | max_depth < 0 = pure []
  | otherwise = reduce Fold.list $ do
    expanded <- liftIO $ concat <$> traverse expand_path source_dirs
    candidate <- select expanded
    liftIO (find_project project_types candidate) >>= \case
      Nothing -> do
        children <- ls candidate
        projects <- liftIO $ find_projects (max_depth - 1) project_types [children]
        select projects
      Just project -> pure project

find_project_commands :: Project -> [Command]
find_project_commands project = concatMap project_commands (Types.project_types project)

expand_path :: FilePath -> IO [FilePath]
expand_path path' = do
  expanded <- wordexp nosubst (encodeString path')
  pure $ either (const []) (map fromString) expanded

sort_projects :: [Project] -> [Project]
sort_projects = sortBy (compare `on` project_path)

-- | Given a path, find matching markers/project type.
-- TODO: Follow symbolic links?
find_project_types :: FilePath -> [ProjectType] -> IO [ProjectType]
find_project_types path' project_types = testdir path' >>= \case
  False -> pure []
  True  -> filterM has_markers project_types
  where has_markers project = case project_markers project of
          [] -> pure True
          xs -> fmap and . traverse (test_marker path') $ xs

project_exec :: Command -> Project -> Select ()
project_exec cmd project = resolve_command project cmd >>= \case
    Selection _ cmd' -> go cmd'
    _ -> liftIO $ throwIO (EmptyError "Command placeholder expansion aborted.")
  where
    go cmd' = liftIO $ IO.hIsTerminalDevice IO.stdin >>= \case
      True  -> run [cmd'] (Just $ project_path project)
      False -> do
        let is_gui = command_gui (command_options cmd)
            path' = Just $ project_path project
        if is_gui
          then do
            shell' <- fromMaybe "bash" <$> need "SHELL"
            spawn (shell' : ["-c", cmd']) path'
          else do
            -- TODO: Add config for terminal
            let terminal = "x-terminal-emulator"
            spawn (terminal : ["-e", cmd']) path'

-- | Test that a marker is valid for a path
test_marker :: FilePath -> ProjectMarker -> IO Bool
test_marker p (ProjectPath marker) = testpath (p </> marker)
test_marker p (ProjectFile marker) = testfile (p </> marker)
test_marker p (ProjectDir  marker) = testdir (p </> marker)
test_marker p (ProjectOr   ms)     = or <$> mapM (test_marker p) ms
test_marker p (ProjectFunc marker) = marker p

-- | Interpolate all command parts into a single text value.
resolve_command :: Project -> Command -> Select (Selection Text)
resolve_command project (Command parts opts) = go [] parts
  where
    go is [] = pure $ Select.selection (T.unwords (reverse is))
    go is (p:ps) = interpolate p >>= \case
      Selection _ i -> go (i:is) ps
      CanceledSelection -> pure CanceledSelection
      EmptySelection    -> pure EmptySelection

    interpolate (TextPart t) = pure $ Select.selection t
    interpolate PathPart = pure $ Select.selection $ format fp $ project_path project
    interpolate DirPart = Select.select $ do
      path' <- lstree (project_path project)
      guard =<< testdir path'
      return $ Select.Identity (format fp path')
    interpolate (ShellPart _ f) = f project
    interpolate (NestedPart ps) = do
      nested <- resolve_command project (Command ps opts)
      pure (format ("\""%s%"\"") <$> nested)
