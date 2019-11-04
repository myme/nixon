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
import           Data.Function (on)
import           Data.List (sortBy)
import           Data.Text (isInfixOf)
import           Envix.Nix
import           Envix.Process
import           Envix.Projects.Types (find_markers)
import           Prelude hiding (FilePath)
import           System.Wordexp
import           Turtle hiding (find, sort, sortBy, toText)

mkproject :: FilePath -> Project
mkproject path = Project (filename path) (parent path) []

data Project = Project { project_name :: FilePath
                       , project_dir :: FilePath
                       , project_commands :: [CmdDesc]
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
find_project_commands path = map (resolve_command path) <$> find_markers path

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
