module Envix.Process
  ( Command (..)
  , Commands
  , CmdDesc (..)
  , CmdDescArg (..)
  , arg
  , arg_fmt
  , build_args
  , command
  , flag
  , from_text
  , resolve_command
  , run
  , spawn
  , to_text
  ) where

import           Data.Maybe (catMaybes)
import qualified Data.Text as T
import           Prelude hiding (FilePath)
import           System.Posix
import           System.Process
import           Turtle hiding (arg, proc)

data CmdDescArg = ArgText Text | ArgPath
                deriving Show

instance IsString CmdDescArg where
  fromString = ArgText . T.pack

data CmdDesc = CmdDesc
  { cmd_desc_command :: Text
  , cmd_desc_args :: [CmdDescArg]
  , cmd_desc_description :: Text
  } deriving Show

data Command = Command { cmd_command :: Text
                       , cmd_args :: [Text]
                       } deriving Show

command :: Text -> [CmdDescArg] -> Text -> CmdDesc
command = CmdDesc

resolve_command :: FilePath -> CmdDesc -> Command
resolve_command path (CmdDesc c a _) = Command c (map expand_arg a)
  where expand_arg (ArgText t) = t
        expand_arg ArgPath = format fp path

from_text :: Text -> Command
from_text cmd = Command (head parts) (tail parts)
  where parts = T.words cmd

to_text :: Command -> Text
to_text cmd = T.unwords $ cmd_command cmd : cmd_args cmd

type Commands = [(Text, Text)]

flag :: a -> Bool -> Maybe [a]
flag key value = if value then Just [key] else Nothing

arg :: Applicative f => a -> a -> f [a]
arg key = pure . ([key] <>) . pure

arg_fmt :: Applicative f => b -> (a -> b) -> a -> f [b]
arg_fmt key f' = pure . ([key] <>) . pure . f'

build_args :: [Maybe [a]] -> [a]
build_args = concat . catMaybes

-- | Run a command and wait for it to finish
run :: Command -> Maybe FilePath -> IO ()
run cmd cwd' = do
  let cwd = T.unpack . format fp <$> cwd'
      cp' = (proc (T.unpack $ cmd_command cmd) (map T.unpack $ cmd_args cmd)) { cwd }
  withCreateProcess cp' $ \_ _ _ handle -> void $ waitForProcess handle

-- | Spawn/fork off a command in the background
spawn :: Command -> Maybe FilePath -> IO ()
spawn cmd cwd' = void $ forkProcess $ do
  _ <- createSession
  run cmd cwd'
