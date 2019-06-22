module Main where

import           Control.Arrow (second)
import qualified Control.Foldl as Fold
import           Control.Monad
import           Data.List (sort)
import           Data.List.NonEmpty (toList)
import qualified Data.Text as T
import           Prelude hiding (FilePath)
import           System.Wordexp
import           Turtle hiding (sort)

nix_files :: [FilePath]
nix_files = ["shell.nix"
            ,"default.nix"
            ]

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

find_projects :: [FilePath] -> IO [FilePath]
find_projects source_dirs = reduce Fold.list $ do
  expanded <- liftIO $ traverse expand_path source_dirs
  candidate <- cat $ map ls (concat expanded)
  is_project' <- liftIO (is_project candidate)
  if not is_project'
    then mzero
    else return candidate

expand_path :: FilePath -> IO [FilePath]
expand_path path = do
  expanded <- wordexp nosubst (encodeString path)
  return $ either (const []) (map fromString) expanded

rofi :: [Text] -> IO (ExitCode, Text)
rofi candidates = do
  let input' = concatMap (toList . textToLines) candidates
  second T.strip <$> procStrict "rofi" ["-dmenu"] (select input')

rofi_format_project_name :: FilePath -> IO Text
rofi_format_project_name path = do
  home' <- home
  let
    name = format fp (basename path)
    pad_width = 30
    name_padded = name <> T.replicate (pad_width - T.length name) " "
    parent' = parent path
    dir = case stripPrefix (home' </> "") parent' of
      Nothing -> parent'
      Just rest -> "~" </> rest
  return $ format (s % " " % fp) name_padded dir

main :: IO ()
main = do
  projects <- find_projects ["/home/mmyrseth/src"]
  let candidates = sort projects
  result <- traverse rofi_format_project_name candidates >>= rofi
  print result
