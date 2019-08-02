module Envix.Projects
  ( Project (..)
  , find_projects
  , project_path
  , mkproject
  ) where

import qualified Control.Foldl as Fold
import           Envix.Nix
import           Prelude hiding (FilePath)
import           System.Wordexp
import           Turtle hiding (sort)

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
