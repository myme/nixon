module Test.Nixon.TestLib
  ( runProc,
    withTmpDir,
  )
where

import Control.Monad.Trans.State (StateT, evalStateT, get)
import Data.IORef (newIORef, readIORef, writeIORef)
import Nixon.Prelude
import Nixon.Process (HasProc (..))
import System.Exit (ExitCode)
import Turtle (runManaged)
import Turtle.Prelude (mktempdir)

newtype MockProc a = MockProc (StateT (ExitCode, Text) IO a)
  deriving (Functor, Applicative, Monad)

runProc :: (ExitCode, Text) -> MockProc a -> IO a
runProc state (MockProc computation) = evalStateT computation state

instance HasProc MockProc where
  proc' _ _ _ = MockProc get

instance MonadIO MockProc where
  liftIO = MockProc . liftIO

withTmpDir :: a -> (FilePath -> IO a) -> IO a
withTmpDir def f = do
  ref <- newIORef def
  runManaged $ do
    path <- mktempdir "/tmp" "nixon-test"
    liftIO (f path >>= writeIORef ref)
  readIORef ref
