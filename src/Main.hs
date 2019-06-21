module Main where

import qualified Control.Foldl as Fold
import           Control.Monad
import           Data.List (sort)
import           Prelude hiding (FilePath)
import           Turtle hiding (sort)

nix_files :: [FilePath]
nix_files = ["shell.nix"
            ,"default.nix"
            ]

project_markers :: [(FilePath -> IO Bool, FilePath)]
project_markers = [(testdir, ".git")
                  ,(testdir, ".hg")
                  ,(testfile, ".project")
                  ]

is_project :: FilePath -> IO Bool
is_project path = do
  is_dir <- isDirectory <$> stat path
  if not is_dir
    then return False
    else or <$> traverse has_marker project_markers
  where has_marker (check, marker) = check (path </> marker)

find_projects :: [FilePath] -> IO [FilePath]
find_projects source_dirs = reduce Fold.list $ do
  candidate <- cat $ map ls source_dirs
  is_project' <- liftIO (is_project candidate)
  if not is_project'
    then mzero
    else return candidate

-- TODO: add wordexp (no command subst) to resolve "~"
-- import System.Wordexp
-- wordexp nosubst path

main :: IO ()
main = do
  projects <- find_projects (fromText <$> ["/home/mmyrseth/src"])
  print (sort projects)
