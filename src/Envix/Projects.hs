module Envix.Projects
  ( Project (..)
  , find_projects
  , find_projects_by_name
  , find_project_commands
  , implode_home
  , mkproject
  , project_exec
  , project_path
  , sort_projects
  ) where

import qualified Control.Foldl as Fold
import           Control.Monad
import           Data.Function (on)
import           Data.List (sortBy)
import           Data.Text (isInfixOf)
import           Envix.Commands
import           Envix.Nix
import           Envix.Process
import           Envix.Projects.Types (ProjectMarker(..), ProjectType, proj, project_marker)
import qualified Envix.Projects.Types as P
import           Prelude hiding (FilePath)
import           System.Wordexp
import           Turtle hiding (find, sort, sortBy, toText)

generic_commands :: [Cmd]
generic_commands = [Cmd "x-terminal-emulator" [] "Terminal"
                   ,Cmd "emacs" [] "Emacs"
                   ,Cmd "vim" [] "Vim"
                   ,Cmd "dolphin" [ArgPath] "Files"
                   ,Cmd "rofi" ["-show", "run"] "Run"
                   ]

-- TODO: Parse e.g. package.json for npm scripts?
-- TODO: Add associated action with each project type
-- e.g. for *.nix invoke nix-shell
--      for .git do a fetch?
--      This should be configurable.
-- This can then be paired up with a `--type <type>` cli arg to allow override
-- which action to run. This can obsolete `--no-nix` with `--type plain`.
project_types :: [ProjectType]
project_types =
  [proj "cabal.project" "Cabal new-style project"
   [Cmd "cabal" ["new-build"] "build"
   ,Cmd "cabal" ["new-repl"] "repl"
   ,Cmd "cabal" ["new-run"] "run"
   ,Cmd "cabal" ["new-test"] "test"
   ]
  ,proj "package.json" "NPM project"
   [Cmd "npm" ["install"] "install"
   ,Cmd "npm" ["start"] "run"
   ,Cmd "npm" ["test"] "test"
   ]
  ,proj ".envrc" "Direnv project"
   [Cmd "direnv" ["allow"] "direnv allow"
   ,Cmd "direnv" ["deny"] "direnv deny"
   ,Cmd "direnv" ["reload"] "direnv reload"
   ]
  ,proj ".git" "Git repository"
   [Cmd "git" ["fetch"] "fetch"
   ,Cmd "git" ["log"] "log"
   ]
  ,proj ".hg" "Mercurial project" []
  ,proj ".project" "Generic project" []
  ] -- ++ map () nix_files

test_marker :: ProjectMarker -> FilePath -> IO Bool
test_marker (ProjectPath marker) path = testpath (path </> marker)
test_marker (ProjectFile marker) path = testfile (path </> marker)
test_marker (ProjectDir  marker) path = testdir (path </> marker)
test_marker (ProjectFunc marker) path = marker path

find_markers :: FilePath -> IO [Cmd]
find_markers path = do
  is_dir <- isDirectory <$> stat path
  if not is_dir
    then return []
    else concatMap P.project_commands <$> filterM has_marker project_types
  where has_marker = (`test_marker` path) . project_marker

mkproject :: FilePath -> Project
mkproject path = Project (filename path) (parent path) []

data Project = Project { project_name :: FilePath
                       , project_dir :: FilePath
                       , project_commands :: [Cmd]
                       } deriving Show

-- | Replace the value of $HOME in a path with "~"
implode_home :: FilePath -> IO FilePath
implode_home path = do
  home' <- home
  return $ case stripPrefix (home' </> "") path of
    Nothing -> path
    Just rest -> "~" </> rest

find_projects_by_name :: FilePath -> [FilePath] -> IO [Project]
find_projects_by_name project = fmap find_matching . find_projects
  where find_matching = filter ((project `isInfix`) . toText . project_name)
        isInfix p = isInfixOf (toText p)
        toText = format fp

find_projects :: [FilePath] -> IO [Project]
find_projects source_dirs = reduce Fold.list $ do
  expanded <- liftIO $ traverse expand_path source_dirs
  candidate <- cat $ map ls (concat expanded)
  commands <- liftIO (find_markers candidate)
  if null commands
    then mzero
    else return Project { project_name = filename candidate
                        , project_dir = parent candidate
                        , project_commands = commands
                        }

find_project_commands :: FilePath -> IO [Command]
find_project_commands path = do
  commands <- find_markers path
  return $ resolve_commands path (generic_commands ++ commands)

expand_path :: FilePath -> IO [FilePath]
expand_path path = do
  expanded <- wordexp nosubst (encodeString path)
  return $ either (const []) (map fromString) expanded

project_path :: Project -> FilePath
project_path project = project_dir project </> project_name project

sort_projects :: [Project] -> [Project]
sort_projects = sortBy (compare `on` project_name)

project_exec :: (Project -> IO ()) -- ^ Non-nix project action
             -> (FilePath -> IO ()) -- ^ Nix project action
             -> Bool -- ^ Use nix
             -> Project -> IO ()
project_exec plain with_nix use_nix project =
  let action = if use_nix
        then find_nix_file (project_path project)
        else pure Nothing
  in action >>= \case
    Nothing -> plain project
    Just nix_file -> with_nix nix_file
