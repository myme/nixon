module Test.Nixon.Command where

import Data.Either
import Nixon.Command
import Test.Hspec

command :: SpecWith ()
command = do
  describe "parse_args" $ do
    it "parses text part" $ do
      parse parse_args "echo 'foo bar baz'" `shouldBe`
        Right []

    it "parses arg part" $ do
      parse parse_args "${arg}" `shouldBe`
        Right [("arg", Env "arg")]

    it "parses text and placeholder part" $ do
      parse parse_args "cat \"${arg}\"" `shouldBe`
        Right [("arg", Env "arg")]

    it "replaces '-' with '_' in $name" $ do
      parse parse_args "cat \"${some-arg}\"" `shouldBe`
        Right [("some_arg", Env "some-arg")]

    it "allows use of $ not matching '${'" $ do
      parse parse_args "echo $SOME_VAR" `shouldBe` Right []

    it "fails on unterminated arg" $ do
      parse parse_args "cat \"${arg\"" `shouldSatisfy` isLeft
