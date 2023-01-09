module Nixon.Command
  ( Command (..),
    CommandLocation (..),
    CommandOutput (..),
    Language (..),
    empty,
    is_bg_command,
    (<!),
    description,
    bg,
    json,
    show_command,
    show_command_with_description,
  )
where

import Data.Text (Text)
import qualified Data.Text as T
import Nixon.Command.Placeholder (Placeholder)
import qualified Nixon.Command.Placeholder as P
import Nixon.Language (Language (None))
import Turtle (FilePath, format, s, (%))
import Prelude hiding (FilePath)

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
    cmdOutput :: CommandOutput,
    -- | Command location in configuration
    cmdLocation :: Maybe CommandLocation
  }
  deriving (Eq, Show)

data CommandLocation = CommandLocation
  { cmdFilePath :: FilePath,
    cmdLineNr :: Int
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
      cmdOutput = Lines,
      cmdLocation = Nothing
    }

data CommandOutput = Lines | JSON deriving (Eq, Show)

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

json :: Bool -> Command -> Command
json j cmd = cmd {cmdOutput = if j then JSON else Lines}

is_bg_command :: Command -> Bool
is_bg_command _ = False
