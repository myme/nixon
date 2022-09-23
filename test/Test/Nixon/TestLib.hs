module Test.Nixon.TestLib where

import Control.Monad.Trans.State (State, evalState, get)
import Data.Text (Text)
import Nixon.Process (HasProc (..))
import System.Exit (ExitCode)

newtype PureProc a = PureProc (State (ExitCode, Text) a)
  deriving (Functor, Applicative, Monad)

runProc :: (ExitCode, Text) -> PureProc a -> a
runProc state (PureProc computation) = evalState computation state

instance HasProc PureProc where
  proc' _ _ _ = PureProc get
