module Nixon.Process
  ( Env
  , arg
  , arg_fmt
  , build_args
  , flag
  , run
  , spawn
  ) where

import           Control.Arrow ((***))
import           Data.Maybe (catMaybes)
import qualified Data.Text as T
import           Prelude hiding (FilePath)
import           System.Posix
import           System.Process hiding (system)
import           Turtle hiding (arg, env, shell)

type Env = [(Text, Text)]

flag :: a -> Bool -> Maybe [a]
flag key value = if value then Just [key] else Nothing

arg :: Applicative f => a -> a -> f [a]
arg key = pure . ([key] <>) . pure

arg_fmt :: Applicative f => b -> (a -> b) -> a -> f [b]
arg_fmt key f' = pure . ([key] <>) . pure . f'

build_args :: [Maybe [a]] -> [a]
build_args = concat . catMaybes

-- | Run a command and wait for it to finish
run :: MonadIO m => [Text] -> Maybe FilePath -> Env -> m ()
run cmd cwd' env' = sh $ do
  currentEnv <- liftIO getEnvironment
  let cp' = shell $ T.unpack (T.unwords cmd)
      cpEnv = Just $ currentEnv ++ map (T.unpack *** T.unpack) env'
  system cp' {
    cwd = T.unpack . format fp <$> cwd',
    env = cpEnv
  } mempty

-- | Spawn/fork off a command in the background
spawn :: MonadIO m => [Text] -> Maybe FilePath -> Env -> m ()
spawn cmd cwd' env' = liftIO $ void $ forkProcess $ do
  _ <- createSession
  run cmd cwd' env'
