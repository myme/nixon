module Main where

import Data.Char (isPrint, isSpace)
import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import Nixon.Prelude
import Nixon.Select
import Nixon.Utils
import Test.Hspec
import Test.Nixon.Backend.Fzf (fzfTests)
import Test.Nixon.Command.Find (findTests)
import Test.Nixon.Config.Markdown
import Test.Nixon.Logging
import Test.Nixon.Process (process)
import Test.QuickCheck
import Test.QuickCheck.Instances.Text ()
import Test.Nixon.Format.Columns (column_tests)

empty :: (Monad m) => a -> m (Selection Text)
empty = const (pure EmptySelection)

arbitraryTextOf :: (Char -> Bool) -> Gen Text
arbitraryTextOf pred' = T.pack <$> listOf1 (arbitrary `suchThat` pred')

-- | newtype for arbitrary whitespace
newtype WsText = WsText {getWs :: Text} deriving (Show, Eq, Ord)

instance Arbitrary WsText where
  shrink (WsText t) = WsText <$> shrink t
  arbitrary = WsText <$> arbitraryTextOf isSpace

-- | newtype for Text without whitespace
newtype NonWsText = NonWsText {getNonWs :: Text} deriving (Show, Eq, Ord)

instance Arbitrary NonWsText where
  shrink (NonWsText t) = NonWsText <$> shrink t
  arbitrary = NonWsText <$> arbitraryTextOf (\c -> isPrint c && (not . isSpace) c)

main :: IO ()
main = hspec $ do
  describe "Backend.Fzf" fzfTests

  describe "Command" $
    describe "Find" findTests

  describe "Config" $ do
    describe "Markdown" markdown_tests

  describe "Format" $ do
    describe "Columns" column_tests

  describe "Logging" logging

  describe "Process" process

  describe "Utils" $ do
    describe "escape" $ do
      it "leaves simple string alone"
        $ escape "foo"
        `shouldBe` "foo"

      it "escapes quote character"
        $ escape "\""
        `shouldBe` "\\\""

      it "escapes backslash character"
        $ escape "\\"
        `shouldBe` "\\\\"

    describe "quote" $ do
      it "surrounds text in quotes"
        $ quote "foo"
        `shouldBe` "\"foo\""

      it "escapes inner text"
        $ quote "\"foo\""
        `shouldBe` "\"\\\"foo\\\"\""

    describe "takeToSpace" $ do
      it "is empty with leading space"
        $ property
        $ \text -> takeToSpace (" " <> getWs text) == ""

      it "reads until first space"
        $ property
        $ \pre ws post -> takeToSpace (getNonWs pre <> getWs ws <> post) == getNonWs pre

    describe "parseColumns" $ do
      it "parses empty input" $ do
        parseColumns [""] `shouldBe` []

      it "parses headers" $ do
        parseColumns ["foo bar baz"] `shouldBe` []

      it "parses headers and values" $ do
        let input = ["foo bar baz", "1 2 3", "4 5 6"]
        parseColumns input
          `shouldBe` [ Map.fromList [("foo", "1"), ("bar", "2"), ("baz", "3")],
                       Map.fromList [("foo", "4"), ("bar", "5"), ("baz", "6")]
                     ]
