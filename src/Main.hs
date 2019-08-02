module Main where

import           Control.Monad
import           Data.Bool (bool)
import           Data.Function (on)
import           Data.List (sort, sortBy)
import qualified Data.Map as Map
import           Data.Maybe (fromMaybe)
import qualified Data.Text as T
import           Data.Text.Read (decimal)
import           Envix.Fzf
import           Envix.Nix
import           Envix.Process hiding (arg)
import           Envix.Projects
import qualified Envix.Rofi as R
import           Envix.Rofi hiding (s, d, i)
import           Prelude hiding (FilePath)
import qualified System.IO as S
import           Turtle hiding (decimal, find, sort, sortBy)

-- | Replace the value of $HOME in a path with "~"
implode_home :: Project -> IO Project
implode_home project = do
  home' <-home
  let
    path = project_dir project
    dir = case stripPrefix (home' </> "") path of
      Nothing -> path
      Just rest -> "~" </> rest
  return $ project { project_dir = dir }

-- | Format project names suited to rofi selection list
rofi_format_project_name :: Project -> IO Text
rofi_format_project_name project = do
  project' <- implode_home project
  let
    pad_width = 30
    name = format fp (project_name project)
    name_padded = name <> T.replicate (pad_width - T.length name) " "
    dir = project_dir project'
  return $ format (s % " <i>" % fp % "</i>") name_padded dir

-- | Build a help message of alternate commands with short description
rofi_build_message :: [(Text, Text)] -> Text
rofi_build_message = T.intercalate ", " . zipWith (curry format_command) [1 :: Int ..]
  where format_command (idx, (_, desc)) = format ("<b>Alt+"%d%"</b>: "%s) idx desc

type Selection = (Project, Command)
type Command = Text
type Commands = [(Text, Text)]

-- | Launch rofi with a list of projects as candidates
rofi_projects :: Commands -> [FilePath] -> IO (Maybe Selection)
rofi_projects commands source_dirs = do
  projects <- sort_projects <$> find_projects source_dirs
  let opts =
        rofi_prompt "Select project" <>
        rofi_format R.i <>
        rofi_markup <>
        rofi_msg (rofi_build_message commands)
      project = (projects !!) . either (const undefined) fst . decimal
  candidates <- traverse rofi_format_project_name projects
  rofi opts candidates >>= \case
    RofiCancel -> return Nothing
    RofiDefault idx -> return $ Just (project idx, "rofi -show run")
    RofiAlternate i idx -> return $ Just (project idx, fst $ commands !! i)
  where
    sort_projects = sortBy (compare `on` project_name)

fzf_format_project_name :: Project -> IO (Text, Project)
fzf_format_project_name project = do
  project' <- implode_home project
  let
    dir = project_dir project'
    name = project_name project'
    path = format fp (dir </> name)
  return (path, project)

fzf_projects :: Commands -> [FilePath] -> IO (Maybe Selection)
fzf_projects commands source_dirs = do
  projects <- find_projects source_dirs
  candidates <- Map.fromList <$> traverse fzf_format_project_name projects
  let opts = fzf_header "Select project"
        <> fzf_border
        -- <> fzf_height 40
  fzf opts (sort $ Map.keys candidates) >>= \case
    FzfCancel -> return Nothing
    FzfDefault out -> return ((, fst (head commands)) <$> Map.lookup out candidates)

data Options = Options { _project :: Maybe Project
                       , _backend :: Maybe Backend
                       , _source_dirs :: [FilePath]
                       , _command :: Maybe Text
                       }

data Backend = Fzf | Rofi

parser :: Parser Options
parser = Options
  <$> optional (arg parse_project "project" "Project to jump into")
  <*> optional (opt parse_backend "backend" 'b' "Backend to use: fzf, rofi")
  <*> many (optPath "path" 'p' "Project directory")
  <*> optional (optText "command" 'c' "Command to run")
  where parse_backend "fzf" = Just Fzf
        parse_backend "rofi" = Just Rofi
        parse_backend _ = Nothing
        parse_project = pure . mkproject . fromText

fzf_exec :: Project -> Text -> IO ()
fzf_exec project _ = find_nix_file (project_path project) >>= \case
  Nothing -> run "bash" [] (Just $ project_dir project)
  Just nix_file -> nix_shell nix_file Nothing

rofi_exec :: Project -> Text -> IO ()
rofi_exec project command = find_nix_file (project_path project) >>= \case
  Nothing -> spawn "bash" ["-c", command] (Just $ project_dir project)
  Just nix_file -> nix_shell_spawn nix_file (Just command)

main :: IO ()
main = do
  opts <- options "Launch project environments" parser
  let source_dirs = if null (_source_dirs opts)
        then ["~/src", "~/projects"]
        else _source_dirs opts
      -- TODO: Allow changing default command
      -- TODO: Allow format strings (%s) in commands to insert e.g. project path
      commands = [("konsole", "Terminal")
                 ,("emacs", "Editor")
                 ,("dolphin", "Files")
                 ]

  def_backend <- bool Rofi Fzf <$> S.hIsTerminalDevice S.stdin
  let backend = fromMaybe def_backend (_backend opts)
      (find, exec) = case backend of
        Fzf -> (fzf_projects, fzf_exec)
        Rofi -> (rofi_projects, rofi_exec)

  find commands source_dirs >>= \case
    Nothing -> putStrLn "No project selected."
    Just (project, command) -> exec project command
