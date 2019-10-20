module Envix.Process
  ( Command
  , Commands
  , arg
  , arg_fmt
  , flag
  , build_args
  , run
  , spawn
  ) where

import           Data.Maybe (catMaybes)
import qualified Data.Text as T
import           Prelude hiding (FilePath)
import           System.Posix
import           System.Process
import           Turtle hiding (arg, proc)

type Command = Text
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
run :: Command -> [Text] -> Maybe FilePath -> IO ()
run command args cwd' = do
  let cwd = T.unpack . format fp <$> cwd'
      cp' = (proc (T.unpack command) (map T.unpack args)) { cwd }
  withCreateProcess cp' $ \_ _ _ handle -> void $ waitForProcess handle

-- | Spawn/fork off a command in the background
spawn :: Command -> [Text] -> Maybe FilePath -> IO ()
spawn command args cwd' = void $ forkProcess $ do
  _ <- createSession
  run command args cwd'
