module Envix.Projects
  ( Project (..)
  , find_projects_by_name
  , find_projects
  , is_project
  , mkproject
  , project_path
  , resolve_project
  , sort_projects
  ) where

import qualified Control.Foldl as Fold
import           Data.Function (on)
import           Data.List (sortBy)
import           Data.Text (isInfixOf)
import           Envix.Nix
import           Prelude hiding (FilePath)
import           System.Wordexp
import           Turtle hiding (find, sort, sortBy, toText)

-- TODO: Add associated action with each project type
-- e.g. for *.nix invoke nix-shell
--      for .git do a fetch?
--      This should be configurable.
-- This can then be paired up with a `--type <type>` cli arg to allow override
-- which action to run. This can obsolete `--no-nix` with `--type plain`.
project_markers :: [(FilePath -> IO Bool, FilePath)]
project_markers = nix_files' ++
                  [(testdir, ".git")
                  ,(testdir, ".hg")
                  ,(testfile, ".project")
                  ]
  where nix_files' = map ((,) testfile) nix_files

is_project :: FilePath -> IO Bool
is_project path = do
  is_dir <- isDirectory <$> stat path
  if not is_dir
    then return False
    else or <$> traverse has_marker project_markers
  where has_marker (check, marker) = check (path </> marker)

mkproject :: FilePath -> Project
mkproject path = Project (filename path) (parent path)

data Project = Project { project_name :: FilePath
                       , project_dir :: FilePath
                       }

find_projects_by_name :: FilePath -> [FilePath] -> IO [Project]
find_projects_by_name project = fmap find_matching . find_projects
  where find_matching = filter ((project `isInfix`) . toText . project_name)
        isInfix p = isInfixOf (toText p)
        toText = format fp

find_projects :: [FilePath] -> IO [Project]
find_projects source_dirs = reduce Fold.list $ do
  expanded <- liftIO $ traverse expand_path source_dirs
  candidate <- cat $ map ls (concat expanded)
  is_project' <- liftIO (is_project candidate)
  if not is_project'
    then mzero
    else return Project { project_name = filename candidate
                        , project_dir = parent candidate
                        }

expand_path :: FilePath -> IO [FilePath]
expand_path path = do
  expanded <- wordexp nosubst (encodeString path)
  return $ either (const []) (map fromString) expanded

project_path :: Project -> FilePath
project_path (Project name dir) = dir </> name

-- | Given a path, resolve it to a project
-- | path can be a relative/absolute path or a name from the project list.
resolve_project :: FilePath -> [FilePath] -> IO [Project]
resolve_project path source_dirs = do
  let project = mkproject path
      name = project_name project
  is_dir <- testdir path
  if is_dir
    then pure [project]
    else find_projects_by_name name source_dirs

sort_projects :: [Project] -> [Project]
sort_projects = sortBy (compare `on` project_name)
