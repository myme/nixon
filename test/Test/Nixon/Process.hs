{-# OPTIONS_GHC -Wno-deferred-out-of-scope-variables #-}

module Test.Nixon.Process where

import Nixon.Process (HasProc (..))
import Test.Hspec
import Test.Nixon.TestLib (runProc)
import Turtle (ExitCode (..))

process :: SpecWith ()
process = do
  it "can simulate success" $ do
    let result = runProc (ExitSuccess, "output") $ do
          proc' "command" ["arg1", "arg2"] mempty
    result `shouldBe` (ExitSuccess, "output")

  it "can simulate failure" $ do
    let result = runProc (ExitFailure 1, "output") $ do
          proc' "command" ["arg1", "arg2"] mempty
    result `shouldBe` (ExitFailure 1, "output")
