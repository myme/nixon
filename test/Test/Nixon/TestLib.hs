module Test.Nixon.TestLib where

import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Trans.State (get, StateT, evalStateT)
import Data.Text (Text)
import Nixon.Process (HasProc (..))
import System.Exit (ExitCode)

newtype MockProc a = MockProc (StateT (ExitCode, Text) IO a)
  deriving (Functor, Applicative, Monad)

runProc :: (ExitCode, Text) -> MockProc a -> IO a
runProc state (MockProc computation) = evalStateT computation state

instance HasProc MockProc where
  proc' _ _ _ = MockProc get

instance MonadIO MockProc where
  liftIO = MockProc . liftIO
