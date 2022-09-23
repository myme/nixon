module Nixon.Process
  ( Cwd,
    Env,
    Run,
    arg,
    arg_fmt,
    build_args,
    flag,
    run,
    run_with_output,
    spawn,
    HasProc (..),
  )
where

import Control.Arrow ((***))
import Data.List.NonEmpty (NonEmpty ((:|)))
import Data.Maybe (catMaybes)
import qualified Data.Text as T
import System.Posix (createSession, forkProcess, getEnvironment)
import System.Process (CreateProcess (cwd, env), proc)
import Turtle
  ( Alternative (empty),
    ExitCode,
    FilePath,
    Line,
    MonadIO (..),
    Text,
    format,
    fp,
    procStrict,
    sh,
    system,
    void,
  )
import Turtle.Shell (Shell)
import Prelude hiding (FilePath)

type Cwd = Maybe FilePath

type Env = [(Text, Text)]

flag :: a -> Bool -> Maybe [a]
flag key value = if value then Just [key] else Nothing

arg :: Applicative f => a -> a -> f [a]
arg key = pure . ([key] <>) . pure

arg_fmt :: Applicative f => b -> (a -> b) -> a -> f [b]
arg_fmt key f' = pure . ([key] <>) . pure . f'

build_args :: [Maybe [a]] -> [a]
build_args = concat . catMaybes

build_cmd :: MonadIO m => NonEmpty Text -> Cwd -> Env -> m CreateProcess
build_cmd cmd cwd' env' = do
  currentEnv <- liftIO getEnvironment
  let (cmd' :| args) = fmap T.unpack cmd
      cpEnv = Just $ currentEnv ++ map (T.unpack *** T.unpack) env'
  pure
    (proc cmd' args)
      { cwd = T.unpack . format fp <$> cwd',
        env = cpEnv
      }

type Run m a = NonEmpty Text -> Cwd -> Env -> m a

-- | Run a command and wait for it to finish
run :: MonadIO m => Run m ()
run cmd cwd' env' = sh $ do
  cmd' <- build_cmd cmd cwd' env'
  system cmd' empty

type Runner m a = CreateProcess -> m a -> m a

-- | Run a process and return the output
run_with_output :: (MonadIO m, Alternative m) => Runner m a -> Run m a
run_with_output stream' cmd cwd' env' = do
  cmd' <- build_cmd cmd cwd' env'
  stream' cmd' empty

-- | Spawn/fork off a command in the background
spawn :: MonadIO m => Run m ()
spawn cmd cwd' env' = liftIO $
  void $
    forkProcess $ do
      _ <- createSession
      run cmd cwd' env'

class Monad m => HasProc m where
  proc' :: Text -> [Text] -> Shell Line -> m (ExitCode, Text)

instance HasProc IO where
  proc' = procStrict
