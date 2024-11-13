module Nixon.Process
  ( Cwd,
    Env,
    RunArgs,
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
import Control.Monad.Trans.Reader (ReaderT)
import qualified Data.ByteString as BS
import Data.List.NonEmpty (NonEmpty ((:|)))
import Data.Maybe (catMaybes)
import qualified Data.Text as T
import Nixon.Prelude
import System.Posix (createSession, forkProcess, getEnvironment)
import System.Process (CreateProcess (cwd, env, std_in), StdStream (CreatePipe), proc)
import Turtle
  ( ExitCode,
    Line,
    format,
    fp,
    procStrict,
    sh,
    system,
    void,
  )
import qualified Turtle
import qualified Turtle.Bytes as BS
import Turtle.Shell (Shell)

type Cwd = Maybe FilePath

type Env = [(Text, Text)]

class HasStdin a where
  getStdin :: Shell a

instance HasStdin Line where
  getStdin = Turtle.stdin

instance HasStdin BS.ByteString where
  getStdin = BS.stdin

flag :: a -> Bool -> Maybe [a]
flag key value = if value then Just [key] else Nothing

arg :: (Applicative f) => a -> a -> f [a]
arg key = pure . ([key] <>) . pure

arg_fmt :: (Applicative f) => b -> (a -> b) -> a -> f [b]
arg_fmt key f' = pure . ([key] <>) . pure . f'

build_args :: [Maybe [a]] -> [a]
build_args = concat . catMaybes

build_cmd :: (MonadIO m) => NonEmpty Text -> Cwd -> Env -> m CreateProcess
build_cmd cmd cwd' env' = do
  currentEnv <- liftIO getEnvironment
  let (cmd' :| args) = fmap T.unpack cmd
      cpEnv = Just $ currentEnv ++ map (T.unpack *** T.unpack) env'
  pure
    (proc cmd' args)
      { cwd = T.unpack . format fp <$> cwd',
        env = cpEnv
      }

type RunArgs b m a =
  -- | Command + args
  NonEmpty Text ->
  -- | Current working directory
  Cwd ->
  -- | Process environment
  Env ->
  -- | Standard input
  Maybe (Shell b) ->
  m a

-- | Run a command and wait for it to finish
run :: (MonadIO m) => RunArgs Line m ()
run cmd cwd' env' stdin = sh $ do
  cmd' <- build_cmd cmd cwd' env'
  case stdin of
    Nothing -> system cmd' getStdin
    Just stdin' -> system cmd' {std_in = CreatePipe} stdin'

type Runner m a = CreateProcess -> Shell a -> m a

-- | Run a process and return the output
run_with_output :: (HasStdin a) => (MonadIO m, Alternative m) => Runner m a -> RunArgs a m a
run_with_output stream' cmd cwd' env' stdin = do
  cmd' <- build_cmd cmd cwd' env'
  case stdin of
    Nothing -> stream' cmd' getStdin
    Just stdin' -> stream' cmd' {std_in = CreatePipe} stdin'

-- | Spawn/fork off a command in the background
spawn :: (MonadIO m) => RunArgs Line m ()
spawn cmd cwd' env' stdin = liftIO
  $ void
  $ forkProcess
  $ do
    _ <- createSession
    run cmd cwd' env' stdin

class (Monad m) => HasProc m where
  proc' :: Text -> [Text] -> Shell Line -> m (ExitCode, Text)

instance HasProc IO where
  proc' = procStrict

instance (MonadIO m) => HasProc (ReaderT a m) where
  proc' cmd args input = liftIO $ proc' cmd args input
