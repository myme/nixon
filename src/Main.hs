module Main where

import           Control.Monad
import           Data.List (sort)
import qualified Data.Text as T
import qualified Data.Text.IO as Tio
import           Envix.Projects
import qualified Envix.Rofi as R
import           Envix.Rofi hiding (s, d, i)
import           Prelude hiding (FilePath)
import           Turtle hiding (sort)

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
  return $ format (s % " <i>" % fp % "</i>") name_padded dir

-- | Build a help message of alternate commands with short description
rofi_build_message :: [(Text, Text)] -> Text
rofi_build_message = T.intercalate ", " . map format_command . zip [1 :: Int ..]
  where format_command (idx, (_, desc)) = format ("<b>Alt+"%d%"</b>: "%s) idx desc

type Commands = [(Text, Text)]

-- | Launch rofi with a list of projects as candidates
rofi_projects :: Commands -> [FilePath] -> IO ()
rofi_projects commands source_dirs = do
  projects <- find_projects source_dirs
  let opts =
        rofi_prompt "Select project" <>
        rofi_format R.i <>
        rofi_markup <>
        rofi_msg (rofi_build_message commands)
  candidates <- sort <$> traverse rofi_format_project_name projects
  rofi opts candidates >>= \case
    RofiCancel -> Tio.putStrLn "Selection canceled"
    RofiDefault p -> Tio.putStrLn $ "Default action: " <> p
    RofiAlternate i p -> Tio.putStrLn $ format ("Alternate action ("%d%"): "%s) i p

main :: IO ()
main = do
  let commands = [("konsole", "Terminal")
                 ,("emacs", "Editor")
                 ,("dolphin", "Files")
                 ]
  rofi_projects commands ["~/src", "~/projects"]
