module Test.Nixon.Command where

import Data.Either
import Nixon.Command
import Test.Hspec

command :: SpecWith ()
command = do
  describe "parse_parts" $ do
    it "parses text part" $ do
      parse parse_parts "echo 'foo bar baz'" `shouldBe`
        Right [TextPart "echo 'foo bar baz'"]

    it "parses placeholder part" $ do
      parse parse_parts "$(nixon placeholder)" `shouldBe`
        Right [Placeholder "placeholder"]

    it "parses text and placeholder part" $ do
      parse parse_parts "cat \"$(nixon placeholder)\"" `shouldBe`
        Right [TextPart "cat \"", Placeholder "placeholder", TextPart "\""]

    it "allows use of $ not matching '$(nixon '" $ do
      parse parse_parts "echo $SOME_VAR" `shouldBe` Right [TextPart "echo $SOME_VAR"]

    it "fails on unterminated placeholder" $ do
      parse parse_parts "cat \"$(nixon placeholder\"" `shouldSatisfy` isLeft

  describe "parse_placeholder" $ do
    it "parses placeholder" $ do
      parse parse_placeholder "$(nixon placeholder)" `shouldBe` Right (Placeholder "placeholder")

    it "fails on regular text" $ do
      parse parse_placeholder "echo \"Foo\"" `shouldSatisfy` isLeft
