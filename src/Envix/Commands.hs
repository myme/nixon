module Envix.Commands
  ( Cmd (..)
  , Argument (..)
  , resolve_commands
  ) where

import qualified Data.Text as T
import           Envix.Process
import           Prelude hiding (FilePath)
import           Turtle

data Argument = TextArg Text | ProjectPath

instance IsString Argument where
  fromString = TextArg . T.pack

data Cmd = Cmd { _cmd :: Text
               , _args :: [Argument]
               , _desc :: Text
               }

resolve_commands :: FilePath -> [Cmd] -> [Command]
resolve_commands path commands = to_command <$> commands
  where
        to_command (Cmd c a _) = Command c (map expand_arg a)
        expand_arg (TextArg t) = t
        expand_arg ProjectPath = format fp path
