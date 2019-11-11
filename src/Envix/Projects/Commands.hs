module Envix.Projects.Commands
  ( CmdDesc (..)
  , CmdDescArg (..)
  , gui
  , term
  , resolve_command
  ) where

import           Data.String (IsString(..))
import qualified Data.Text as T
import           Envix.Process
import           Prelude hiding (FilePath)
import           Turtle

-- | Arguments to command descriptions.
-- | Can support interpolation of e.g. project path.
data CmdDescArg = ArgText Text | ArgPath
                deriving Show

instance IsString CmdDescArg where
  fromString = ArgText . T.pack

-- | Command type, either graphical or terminal.
data CmdType = Terminal | GUI deriving Show

-- | Command description.
data CmdDesc = CmdDesc
  { cmd_desc_type :: CmdType
  , cmd_desc_command :: Text
  , cmd_desc_args :: [CmdDescArg]
  , cmd_desc_description :: Text
  } deriving Show

term :: Text -> [CmdDescArg] -> Text -> CmdDesc
term = CmdDesc Terminal

gui :: Text -> [CmdDescArg] -> Text -> CmdDesc
gui = CmdDesc GUI

resolve_command :: FilePath -> CmdDesc -> Command
resolve_command path (CmdDesc _ c a _) = Command c (map expand_arg a)
  where expand_arg (ArgText t) = t
        expand_arg ArgPath = format fp path
