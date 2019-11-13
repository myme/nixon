module Main where

import Envix.Projects
import Envix.Projects.Types
import Envix.Select
import Test.Hspec

main :: IO ()
main = hspec $ do
  describe "Projects" $
    describe "resolve_commands" $ do
      let project = from_path "/foo/bar/baz"
          empty = const (pure EmptySelection)

      it "joins parts with spaces" $ do
        res <- runSelect empty $ resolve_command project "this is a command"
        res `shouldBe` "this is a command"

      it "joins parts with spaces" $ do
        res <- runSelect empty $ resolve_command project ("this is:" <> path)
        res `shouldBe` "this is: /foo/bar/baz"
