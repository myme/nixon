module Test.Nixon.Logging (logging) where

import Control.Monad.Trans.RWS
import Data.Text
import Nixon.Logging
import Nixon.Prelude
import Test.Hspec
import Test.QuickCheck
import Test.QuickCheck.Instances.Text ()

newtype TestLogging a = TestLogging (RWS LogLevel [Text] () a)
  deriving (Functor, Applicative, Monad)

runLogger :: LogLevel -> TestLogging a -> [Text]
runLogger lvl (TestLogging x) = snd $ execRWS x lvl ()

instance HasLogging TestLogging where
  loglevel = TestLogging ask
  logout x = TestLogging (tell [x])

logging :: SpecWith ()
logging = do
  let genInput :: Gen (LogLevel, Text)
      genInput = (,) <$> arbitraryBoundedEnum <*> arbitrary

      test log_fn min_lvl = forAll genInput $ \(lvl, txt) ->
        let expected = if lvl <= min_lvl then [txt] else []
        in runLogger lvl (log_fn txt) == expected

  describe "filters log messages" $ do
    it "log_debug" $ test log_debug LogDebug
    it "log_info"  $ test log_info  LogInfo
    it "log_warn"  $ test log_warn  LogWarning
    it "log_error" $ test log_error LogError
