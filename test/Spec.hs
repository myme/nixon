module Main where

import Envix.Projects.Types
import Test.Hspec

main :: IO ()
main = hspec $ do
  describe "Projects" $
    describe "resolve_commands" $ do
      let project = from_path "/foo/bar/baz"

      it "joins parts with spaces" $ do
        resolve_command project "this is a command" `shouldBe` "this is a command"

      it "joins parts with spaces" $ do
        resolve_command project ("this is:" <> path) `shouldBe` "this is: /foo/bar/baz"
