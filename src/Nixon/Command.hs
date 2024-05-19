module Nixon.Command
  ( Command (..),
    CommandLocation (..),
    Language (..),
    empty,
    is_bg_command,
    (<!),
    description,
    bg,
    show_command,
    show_command_with_description,
  )
where

import qualified Data.Text as T
import Nixon.Command.Placeholder (Placeholder)
import qualified Nixon.Command.Placeholder as P
import Nixon.Language (Language (None))
import Nixon.Prelude
import Turtle (format, s, (%))

data Command = Command
  { cmdName :: Text,
    cmdDesc :: Maybe Text,
    cmdLang :: Language,
    cmdProjectTypes :: [Text],
    cmdSource :: Text,
    cmdPwd :: Maybe FilePath,
    cmdPlaceholders :: [Placeholder],
    cmdIsBg :: Bool,
    -- | Command should be hidden from selection
    cmdIsHidden :: Bool,
    -- | Command location in configuration
    cmdLocation :: Maybe CommandLocation
  }
  deriving (Eq, Show)

data CommandLocation = CommandLocation
  { cmdFilePath :: FilePath,
    cmdStartLine :: Int,
    cmdEndLine :: Int,
    cmdLevel :: Int
  }
  deriving (Eq, Show)

empty :: Command
empty =
  Command
    { cmdName = "",
      cmdDesc = Nothing,
      cmdLang = None,
      cmdProjectTypes = [],
      cmdSource = "",
      cmdPwd = Nothing,
      cmdPlaceholders = [],
      cmdIsBg = False,
      cmdIsHidden = False,
      cmdLocation = Nothing
    }

show_command :: Command -> Text
show_command cmd = T.unwords $ cmdName cmd : map (format ("${" % s % "}") . P.name) (cmdPlaceholders cmd)

show_command_with_description :: Command -> Text
show_command_with_description cmd = format (s % s) (cmdName cmd) desc
  where
    desc = case cmdDesc cmd of
      Nothing -> ""
      Just txt -> format (" - " % s) txt

(<!) :: a -> (a -> b) -> b
(<!) cmd op = op cmd

description :: Text -> Command -> Command
description d cmd = cmd {cmdDesc = Just d}

bg :: Bool -> Command -> Command
bg g cmd = cmd {cmdIsBg = g}

is_bg_command :: Command -> Bool
is_bg_command _ = False
