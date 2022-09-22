module Main where

import           Data.Char (isSpace, isPrint)
import           Data.Text (Text)
import qualified Data.Text as T
import           Nixon.Select
import           Nixon.Utils
import           Test.Hspec
import           Test.Nixon.Command
import           Test.Nixon.Config.Markdown
import           Test.Nixon.Logging
import           Test.QuickCheck
import           Test.QuickCheck.Instances.Text ()
import Test.Nixon.Backend.Fzf (fzf)

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
  describe "Backend.Fzf" fzf

  describe "Command" command

  describe "Config" $ do
    describe "Markdown" markdown_tests

  describe "Logging" logging

  describe "Utils" $ do
    describe "escape" $ do
      it "leaves simple string alone" $
        escape "foo" `shouldBe` "foo"

      it "escapes quote character" $
        escape "\"" `shouldBe` "\\\""

      it "escapes backslash character" $
        escape "\\" `shouldBe` "\\\\"

    describe "quote" $ do
      it "surrounds text in quotes" $
        quote "foo" `shouldBe` "\"foo\""

      it "escapes inner text" $
        quote "\"foo\"" `shouldBe` "\"\\\"foo\\\"\""

    describe "takeToSpace" $ do
      it "is empty with leading space" $
        property $ \text -> takeToSpace (" " <> getWs text) == ""

      it "reads until first space" $
        property $ \pre ws post -> takeToSpace (getNonWs pre <> getWs ws <> post) == getNonWs pre
