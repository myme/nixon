module Main where

import           Control.Monad
import           Data.Function (on)
import           Data.List (sortBy)
import qualified Data.Text as T
import qualified Data.Text.IO as Tio
import           Data.Text.Read (decimal)
import           Envix.Projects
import qualified Envix.Rofi as R
import           Envix.Rofi hiding (s, d, i)
import           Prelude hiding (FilePath)
import           Turtle hiding (decimal, sortBy)

-- | Format project names suited to rofi selection list
rofi_format_project_name :: Project -> IO Text
rofi_format_project_name project = do
  home' <- home
  let
    name = format fp (project_name project)
    pad_width = 30
    name_padded = name <> T.replicate (pad_width - T.length name) " "
    pdir = project_dir project
    dir = case stripPrefix (home' </> "") pdir of
      Nothing -> pdir
      Just rest -> "~" </> rest
  return $ format (s % " <i>" % fp % "</i>") name_padded dir

-- | Build a help message of alternate commands with short description
rofi_build_message :: [(Text, Text)] -> Text
rofi_build_message = T.intercalate ", " . map format_command . zip [1 :: Int ..]
  where format_command (idx, (_, desc)) = format ("<b>Alt+"%d%"</b>: "%s) idx desc

type Commands = [(Text, Text)]

-- | Launch rofi with a list of projects as candidates
rofi_projects :: Commands -> [FilePath] -> IO ()
rofi_projects commands source_dirs = do
  projects <- sortBy (compare `on` project_name) <$> find_projects source_dirs
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

main :: IO ()
main = rofi_projects commands ["~/src", "~/projects"]
  where commands = [("konsole", "Terminal")
                   ,("emacs", "Editor")
                   ,("dolphin", "Files")
                   ]
