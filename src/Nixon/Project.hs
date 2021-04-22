module Nixon.Project
  ( Project (..)
  , ProjectType (..)
  , ProjectMarker (..)
  , Command
  , find_in_project
  , find_in_project_or_default
  , find_projects
  , find_projects_by_name
  , from_path
  , mkproject
  , parents
  , proj
  , project_path
  , sort_projects
  ) where

import qualified Control.Foldl as Fold
import           Control.Monad (filterM)
import           Data.Function (on)
import           Data.List (intercalate, sortBy)
import           Data.Maybe (fromMaybe)
import           Data.Text (isInfixOf)
import qualified Data.Text as T
import           Nixon.Command (Command(..))
import           Prelude hiding (FilePath)
import           System.Wordexp
import           Turtle hiding (f, find, sort, sortBy, text, toText)

data Project = Project { project_name :: FilePath
                       , project_dir :: FilePath
                       , project_types :: [ProjectType]
                       } deriving Show

from_path :: FilePath -> Project
from_path path' = Project { project_name = filename path'
                          , project_dir = parent path'
                          , project_types = []
                          }

-- | Full path to a project
project_path :: Project -> FilePath
project_path project = project_dir project </> project_name project

data ProjectType = ProjectType { project_id :: Text
                               , project_markers :: [ProjectMarker]
                               , project_description :: Text
                               } deriving (Eq, Show)

-- | Construct a project description
proj :: Text -> [ProjectMarker] -> Text -> ProjectType
proj = ProjectType

data ProjectMarker = ProjectPath FilePath -- ^ Check if path exists
                   | ProjectFile FilePath -- ^ Check if path is a file
                   | ProjectDir FilePath -- ^ Check if path is a directory
                   | ProjectOr [ProjectMarker] -- ^ Logical `or` two marker checks
                   | ProjectFunc (FilePath -> IO Bool) -- ^ Run arbitrary check on candidate dir

instance IsString ProjectMarker where
  fromString = ProjectPath . fromText . T.pack

instance Eq ProjectMarker where
  (==) (ProjectPath p) (ProjectPath q) = p == q
  (==) (ProjectFile p) (ProjectFile q) = p == q
  (==) (ProjectDir p) (ProjectDir q) = p == q
  (==) (ProjectOr p) (ProjectOr q) = all (uncurry (==)) $ zip p q
  (==) _ _ = False

instance Show ProjectMarker where
  show (ProjectFunc _) = "ProjectFunc (..)"
  show (ProjectOr ms)  = "ProjectOr (" ++ intercalate ", " (map show ms) ++ ")"
  show (ProjectPath p) = "ProjectPath" ++ show p
  show (ProjectFile p) = "ProjectFile" ++ show p
  show (ProjectDir p)  = "ProjectDir"  ++ show p

mkproject :: FilePath -> Project
mkproject path' = Project (filename path') (parent path') []

parents :: FilePath -> [FilePath]
parents path'
  | path' == parent path' = [path']
  | otherwise = path' : parents (parent path')

-- | Find/filter out a project in which path is a subdirectory.
find_in_project :: MonadIO m => [ProjectType] -> FilePath -> m (Maybe Project)
find_in_project project_types path' = liftIO $ find_project project_types path' >>= \case
    Nothing -> if parent path' == root path'
      then pure Nothing
      else find_in_project project_types (parent path')
    project -> pure project

find_in_project_or_default :: MonadIO m => [ProjectType] -> FilePath -> m Project
find_in_project_or_default project_types path' = do
  types <- liftIO $ find_project_types path' project_types
  let current = (from_path path') { project_types = types }
  fromMaybe current <$> find_in_project project_types path'

find_projects_by_name :: MonadIO m => FilePath -> [ProjectType] -> [FilePath] -> m [Project]
find_projects_by_name project project_types = liftIO . fmap find_matching . find_projects 1 project_types
  where find_matching = filter ((project `isInfix`) . toText . project_name)
        isInfix p = isInfixOf (toText p)
        toText = format fp

find_project :: MonadIO m => [ProjectType] -> FilePath -> m (Maybe Project)
find_project project_types source_dir = do
  isdir <- testdir source_dir
  if not isdir
    then pure Nothing
    else do
      types <- find_project_types source_dir project_types
      if all (null . project_markers) types
        then pure Nothing
        else pure $ Just Project
          { project_name = filename source_dir
          , project_dir = parent source_dir
          , project_types = types
          }

-- | Find projects from a list of source directories.
--
-- Filepath expansion is done on each source directory. For each source
-- directory not a project, look for subdirs being projects.
find_projects :: MonadIO m => Integer -> [ProjectType] -> [FilePath] -> m [Project]
find_projects max_depth project_types source_dirs
  | max_depth < 0 = pure []
  | otherwise = reduce Fold.list $ do
    expanded <- liftIO $ concat <$> traverse expand_path source_dirs
    candidate <- select expanded
    guard =<< testdir candidate
    liftIO (find_project project_types candidate) >>= \case
      Nothing -> do
        children <- ls candidate
        projects <- liftIO $ find_projects (max_depth - 1) project_types [children]
        select projects
      Just project -> pure project

expand_path :: MonadIO m => FilePath -> m [FilePath]
expand_path path' = do
  expanded <- liftIO $ wordexp nosubst (encodeString path')
  pure $ either (const []) (map fromString) expanded

sort_projects :: [Project] -> [Project]
sort_projects = sortBy (compare `on` project_path)

-- | Given a path, find matching markers/project type.
find_project_types :: MonadIO m => FilePath -> [ProjectType] -> m [ProjectType]
find_project_types path' project_types = liftIO $ testdir path' >>= \case
  False -> pure []
  True  -> filterM has_markers project_types
  where has_markers project = case project_markers project of
          [] -> pure True
          xs -> fmap and . traverse (test_marker path') $ xs

-- | Test that a marker is valid for a path
test_marker :: MonadIO m => FilePath -> ProjectMarker -> m Bool
test_marker p (ProjectPath marker) = testpath (p </> marker)
test_marker p (ProjectFile marker) = testfile (p </> marker)
test_marker p (ProjectDir  marker) = testdir (p </> marker)
test_marker p (ProjectOr   ms)     = or <$> mapM (test_marker p) ms
test_marker p (ProjectFunc marker) = liftIO $ marker p
