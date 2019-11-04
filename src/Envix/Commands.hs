module Envix.Commands
  ( Cmd (..)
  , Argument (..)
  , resolve_commands
  ) where

import qualified Data.Text as T
import           Envix.Process
import           Prelude hiding (FilePath)
import           Turtle

data Argument = ArgText Text | ArgPath
              deriving Show

instance IsString Argument where
  fromString = ArgText . T.pack

data Cmd = Cmd { _cmd :: Text
               , _args :: [Argument]
               , _desc :: Text
               } deriving Show

resolve_commands :: FilePath -> [Cmd] -> [Command]
resolve_commands path commands = to_command <$> commands
  where
        to_command (Cmd c a _) = Command c (map expand_arg a)
        expand_arg (ArgText t) = t
        expand_arg ArgPath = format fp path
