module Main where

import           Data.Char (isSpace, isPrint)
import           Data.Text (Text)
import qualified Data.Text as T
import           Nixon.Command ( Command(..), Part(..), path, shell)
import           Nixon.Project
import           Nixon.Project.Types
import           Nixon.Select
import           Nixon.Utils
import           Test.Hspec
import           Test.Nixon.Logging
import           Test.QuickCheck
import           Test.QuickCheck.Instances.Text ()

empty :: Monad m => a -> m (Selection Text)
empty = const (pure EmptySelection)

arbitraryTextOf :: (Char -> Bool) -> Gen Text
arbitraryTextOf pred' = T.pack <$> listOf1 (arbitrary `suchThat` pred')

-- | newtype for arbitrary whitespace
newtype WsText = WsText { getWs :: Text } deriving (Show, Eq, Ord)
instance Arbitrary WsText where
  shrink (WsText t) = WsText <$> shrink t
  arbitrary = WsText <$> arbitraryTextOf isSpace


-- | newtype for Text without whitespace
newtype NonWsText = NonWsText { getNonWs :: Text } deriving (Show, Eq, Ord)
instance Arbitrary NonWsText where
  shrink (NonWsText t) = NonWsText <$> shrink t
  arbitrary = NonWsText <$> arbitraryTextOf (\c -> isPrint c && (not . isSpace) c)

main :: IO ()
main = hspec $ do
  describe "Logging" logging
  describe "Project" $
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

  describe "Utils" $
    describe "takeToSpace" $ do
      it "is empty with leading space" $
        property $ \text -> takeToSpace (" " <> getWs text) == ""

      it "reads until first space" $
        property $ \pre ws post -> takeToSpace (getNonWs pre <> getWs ws <> post) == getNonWs pre
