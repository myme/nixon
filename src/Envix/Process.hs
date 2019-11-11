module Envix.Process
  ( arg
  , arg_fmt
  , build_args
  , flag
  , run
  , spawn
  ) where

import           Data.Maybe (catMaybes)
import qualified Data.Text as T
import           Prelude hiding (FilePath)
import           System.Posix
import           Turtle hiding (arg, proc)

flag :: a -> Bool -> Maybe [a]
flag key value = if value then Just [key] else Nothing

arg :: Applicative f => a -> a -> f [a]
arg key = pure . ([key] <>) . pure

arg_fmt :: Applicative f => b -> (a -> b) -> a -> f [b]
arg_fmt key f' = pure . ([key] <>) . pure . f'

build_args :: [Maybe [a]] -> [a]
build_args = concat . catMaybes

-- | Run a command and wait for it to finish
run :: [Text] -> Maybe FilePath -> IO ()
run cmd cwd' = sh $ do
  maybe (pure ()) pushd cwd'
  void $ shell (T.concat cmd) mempty

-- | Spawn/fork off a command in the background
spawn :: [Text] -> Maybe FilePath -> IO ()
spawn cmd cwd' = void $ forkProcess $ do
  _ <- createSession
  run cmd cwd'
