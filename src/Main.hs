module Main where

import           Control.Monad
import           Data.Function (on)
import           Data.List (sortBy)
import qualified Data.Text as T
import qualified Data.Text.IO as Tio
import           Data.Text.Read (decimal)
import           Envix.Fzf
import           Envix.Projects
import qualified Envix.Rofi as R
import           Envix.Rofi hiding (s, d, i)
import           Prelude hiding (FilePath)
import           Turtle hiding (decimal, sortBy)

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
rofi_build_message = T.intercalate ", " . map format_command . zip [1 :: Int ..]
  where format_command (idx, (_, desc)) = format ("<b>Alt+"%d%"</b>: "%s) idx desc

type Commands = [(Text, Text)]

sort_projects :: [Project] -> [Project]
sort_projects = sortBy (compare `on` project_name)

-- | Launch rofi with a list of projects as candidates
rofi_projects :: Commands -> [FilePath] -> IO ()
rofi_projects commands source_dirs = do
  projects <- sort_projects <$> find_projects source_dirs
  let opts =
        rofi_prompt "Select project" <>
        rofi_format R.i <>
        rofi_markup <>
        rofi_msg (rofi_build_message commands)
      project = project_name . (projects !!) . either (const undefined) fst . decimal
  candidates <- traverse rofi_format_project_name projects
  rofi opts candidates >>= \case
    RofiCancel -> Tio.putStrLn "Selection canceled"
    RofiDefault idx -> Tio.putStrLn $ format ("Default action: "%fp) (project idx)
    RofiAlternate i idx -> Tio.putStrLn $ format ("Alternate action #"%d%": "%fp) i (project idx)

fzf_format_project_name :: Project -> IO Text
fzf_format_project_name project = do
  project' <- implode_home project
  let
    dir = project_dir project'
    name = project_name project'
  return $ format fp (dir </> name)

fzf_projects :: [FilePath] -> IO ()
fzf_projects source_dirs = do
  projects <- sort_projects <$> find_projects source_dirs
  candidates <- traverse fzf_format_project_name projects
  let opts = fzf_header "Select project" <>
        fzf_border <>
        fzf_height 40
  fzf opts candidates

data Options = Options { _backend :: Backend
                       , _source_dirs :: [FilePath]
                       }

data Backend = Fzf | Rofi

parser :: Parser Options
parser = Options
  <$> arg parse_backend "backend" "Backend to use: fzf, rofi"
  <*> (many $ optPath "path" 'p' "Project directory")
  where parse_backend "fzf" = Just Fzf
        parse_backend "rofi" = Just Rofi
        parse_backend _ = Nothing

main :: IO ()
main = do
  opts <- options "Launch project environments" parser
  let source_dirs = if null (_source_dirs opts)
        then ["~/src", "~/projects"]
        else _source_dirs opts
      commands = [("konsole", "Terminal")
                 ,("emacs", "Editor")
                 ,("dolphin", "Files")
                 ]
  case _backend opts of
    Fzf -> fzf_projects source_dirs
    Rofi -> rofi_projects commands source_dirs
