module Nixon.Project
  ( Project (..),
    ProjectType (..),
    ProjectMarker (..),
    Command,
    find_in_project,
    find_in_project_or_default,
    find_projects,
    find_projects_by_name,
    from_path,
    mkproject,
    parents,
    proj,
    project_path,
    sort_projects,
  )
where

import Control.Applicative (empty)
import qualified Control.Foldl as Fold
import Control.Monad (filterM)
import Data.Function (on)
import Data.List (intercalate, sortBy)
import Data.Maybe (fromMaybe)
import qualified Data.Text as T
import Nixon.Command (Command (..))
import Nixon.Prelude
import Nixon.Utils (fromPath, fromText)
import System.Wordexp (nosubst, wordexp)
import Turtle
  ( IsString (..),
    filename,
    format,
    fp,
    guard,
    ls,
    parent,
    reduce,
    root,
    select,
    testdir,
    testfile,
    testpath,
    (</>),
  )

data Project = Project
  { projectName :: FilePath,
    projectDir :: FilePath,
    projectTypes :: [ProjectType]
  }
  deriving (Eq, Show)

from_path :: FilePath -> Project
from_path path' =
  Project
    { projectName = filename path',
      projectDir = parent path',
      projectTypes = []
    }

-- | Full path to a project
project_path :: Project -> FilePath
project_path project = projectDir project </> projectName project

data ProjectType = ProjectType
  { project_id :: Text,
    project_markers :: [ProjectMarker],
    project_description :: Text
  }
  deriving (Eq, Show)

-- | Construct a project description
proj :: Text -> [ProjectMarker] -> Text -> ProjectType
proj = ProjectType

data ProjectMarker
  = -- | Check if path exists
    ProjectPath FilePath
  | -- | Check if path is a file
    ProjectFile FilePath
  | -- | Check if path is a directory
    ProjectDir FilePath
  | -- | Logical `or` two marker checks
    ProjectOr [ProjectMarker]
  | -- | Run arbitrary check on candidate dir
    ProjectFunc (FilePath -> IO Bool)

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
  show (ProjectOr ms) = "ProjectOr (" ++ intercalate ", " (map show ms) ++ ")"
  show (ProjectPath p) = "ProjectPath" ++ show p
  show (ProjectFile p) = "ProjectFile" ++ show p
  show (ProjectDir p) = "ProjectDir" ++ show p

mkproject :: FilePath -> Project
mkproject path' = Project (filename path') (parent path') []

parents :: FilePath -> [FilePath]
parents path'
  | path' == parent path' = [path']
  | otherwise = path' : parents (parent path')

-- | Find/filter out a project in which path is a subdirectory.
find_in_project :: MonadIO m => [ProjectType] -> FilePath -> m (Maybe Project)
find_in_project ptypes path' =
  liftIO $
    find_project ptypes path' >>= \case
      Nothing ->
        if parent path' == root path'
          then pure Nothing
          else find_in_project ptypes (parent path')
      project -> pure project

find_in_project_or_default :: MonadIO m => [ProjectType] -> FilePath -> m Project
find_in_project_or_default ptypes path' = do
  types <- liftIO $ find_project_types path' ptypes
  let current = (from_path path') {projectTypes = types}
  fromMaybe current <$> find_in_project ptypes path'

find_projects_by_name :: MonadIO m => FilePath -> [ProjectType] -> [FilePath] -> m [Project]
find_projects_by_name project ptypes = liftIO . fmap find_matching . find_projects 1 ptypes
  where
    find_matching = filter ((project `isInfix`) . toText . projectName)
    isInfix p = T.isInfixOf (toText p)
    toText = format fp

find_project :: MonadIO m => [ProjectType] -> FilePath -> m (Maybe Project)
find_project ptypes source_dir = do
  isdir <- testdir source_dir
  if not isdir
    then pure Nothing
    else do
      types <- find_project_types source_dir ptypes
      if all (null . project_markers) types
        then pure Nothing
        else
          pure $
            Just
              Project
                { projectName = filename source_dir,
                  projectDir = parent source_dir,
                  projectTypes = types
                }

-- | Find projects from a list of source directories.
--
-- Filepath expansion is done on each source directory. For each source
-- directory not a project, look for subdirs being projects.
find_projects :: MonadIO m => Integer -> [ProjectType] -> [FilePath] -> m [Project]
find_projects max_depth ptypes source_dirs
  | max_depth < 0 = pure []
  | otherwise = reduce Fold.mconcat $ do
    expanded <- liftIO $ concat <$> traverse expand_path source_dirs
    candidate <- select expanded
    guard =<< testdir candidate
    let project = maybe empty pure <$> liftIO (find_project ptypes candidate)
        subprojects = do
          children <- ls candidate
          liftIO $ find_projects (max_depth - 1) ptypes [children]
    project <|> subprojects

expand_path :: MonadIO m => FilePath -> m [FilePath]
expand_path path' = do
  expanded <- liftIO $ wordexp nosubst (fromPath path')
  pure $ either (const []) (map fromString) expanded

sort_projects :: [Project] -> [Project]
sort_projects = sortBy (compare `on` project_path)

-- | Given a path, find matching markers/project type.
find_project_types :: MonadIO m => FilePath -> [ProjectType] -> m [ProjectType]
find_project_types path' ptypes =
  liftIO $
    testdir path' >>= \case
      False -> pure []
      True -> filterM has_markers ptypes
  where
    has_markers project = case project_markers project of
      [] -> pure True
      xs -> fmap and . traverse (test_marker path') $ xs

-- | Test that a marker is valid for a path
test_marker :: MonadIO m => FilePath -> ProjectMarker -> m Bool
test_marker p (ProjectPath marker) = testpath (p </> marker)
test_marker p (ProjectFile marker) = testfile (p </> marker)
test_marker p (ProjectDir marker) = testdir (p </> marker)
test_marker p (ProjectOr ms) = or <$> mapM (test_marker p) ms
test_marker p (ProjectFunc marker) = liftIO $ marker p
