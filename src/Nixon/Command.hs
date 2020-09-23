module Nixon.Command
  ( Command(..)
  , list_commands
  , show_command
  , is_gui_command
  ) where

import Data.Text (unpack, Text, intercalate)
import Turtle

data Command = Command
  { cmdName :: Text
  , cmdDesc :: Maybe Text
  , cmdLang :: Maybe Text
  , cmdProjectTypes :: [Text]
  , cmdSrc :: Text
  , cmdIsGui :: Bool
  }


instance Show Command where
  show = unpack . show_command


show_command :: Command -> Text
show_command (Command name _ lang projectTypes src _) = format fmt name pt (maybe "" id lang) src
  where fmt = s%" ["%s%"]:\n"%"#!"%s%"\n"%s
        pt = intercalate ", " projectTypes


list_commands :: [Command] -> Text
list_commands = intercalate "\n\n" . map show_command


is_gui_command :: Command -> Bool
is_gui_command _ = False
