module Nixon.Process
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
import           System.Process hiding (system)
import           Turtle hiding (arg, shell)

flag :: a -> Bool -> Maybe [a]
flag key value = if value then Just [key] else Nothing

arg :: Applicative f => a -> a -> f [a]
arg key = pure . ([key] <>) . pure

arg_fmt :: Applicative f => b -> (a -> b) -> a -> f [b]
arg_fmt key f' = pure . ([key] <>) . pure . f'

build_args :: [Maybe [a]] -> [a]
build_args = concat . catMaybes

-- | Run a command and wait for it to finish
run :: MonadIO m => [Text] -> Maybe FilePath -> m ()
run cmd cwd' = sh $ do
  let cp' = shell (T.unpack $ T.intercalate " " cmd)
  maybe (pure ()) pushd cwd'
  system cp' mempty

-- | Spawn/fork off a command in the background
spawn :: MonadIO m => [Text] -> Maybe FilePath -> m ()
spawn cmd cwd' = liftIO $ void $ forkProcess $ do
  _ <- createSession
  run cmd cwd'
