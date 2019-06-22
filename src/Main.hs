module Main where

import qualified Control.Foldl as Fold
import           Control.Monad
import           Data.List (sort)
import qualified Data.Text as T
import           Prelude hiding (FilePath)
import           Rofi hiding (s, d)
import qualified Rofi as R
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

-- | Format project names suited to rofi selection list
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

-- | Build a help message of alternate commands with short description
rofi_build_message :: [(Text, Text)] -> Text
rofi_build_message = T.intercalate ", " . map format_command . zip [1 :: Int ..]
  where format_command (idx, (_, desc)) = format ("<b>Alt+"%d%"</b>: "%s) idx desc

-- | Launch rofi with a list of projects as candidates
rofi_projects :: [FilePath] -> IO ()
rofi_projects source_dirs = do
  projects <- find_projects source_dirs
  let candidates = traverse rofi_format_project_name (sort projects)
      opts =
        rofi_prompt "Select project" <>
        rofi_format R.q <>
        rofi_msg (rofi_build_message
                  [("konsole", "Terminal")
                  ,("emacs", "Editor")
                  ,("dolphin", "Files")
                  ])
  result <- candidates >>= rofi opts
  print result

main :: IO ()
main = rofi_projects ["~/src", "~/projects"]
