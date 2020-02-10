module Main where

import Data.Text (Text)
import Nixon.Projects
import Nixon.Projects.Types
import Nixon.Select
import Test.Hspec

empty :: Monad m => a -> m (Selection Text)
empty = const (pure EmptySelection)

main :: IO ()
main = hspec $ do
  describe "Projects" $
    describe "resolve_command" $ do
      let project = from_path "/foo/bar/baz"

      it "joins parts with spaces" $ do
        res <- runSelect empty $ resolve_command project "this is a command"
        res `shouldBe` selection "this is a command"

      it "joins parts with spaces" $ do
        res <- runSelect empty $ resolve_command project ("this is:" <> path)
        res `shouldBe` selection "this is: /foo/bar/baz"

      it "shell aborts on empty selection" $ do
        res <- runSelect empty $ do
          let part = shell "empty" empty
          resolve_command project ("this is:" <> part)
        res `shouldBe` EmptySelection

      it "nested aborts on empty selection" $ do
        res <- runSelect empty $ do
          let part = Command [NestedPart [ShellPart "empty" empty]] mempty
          resolve_command project ("this is:" <> part)
        res `shouldBe` EmptySelection
