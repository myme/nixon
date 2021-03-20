{-# LANGUAGE LambdaCase #-}

module Test.Nixon.Config.Markdown where

import           Data.Text (Text)
import qualified Data.Text as T
import qualified Nixon.Command as Cmd
import           Nixon.Config.Markdown (parseMarkdown)
import           Nixon.Config.Types (defaultConfig)
import qualified Nixon.Config.Types as Cfg
import           Test.Hspec

match_error :: Text -> Either Text b -> Bool
match_error match = either (T.isInfixOf match) (const False)

markdown_tests :: SpecWith ()
markdown_tests = do
  describe "parseMarkdown" $ do
    describe "config section" $ do
      it "allows empty JSON object" $ do
        let result = parseMarkdown $ T.unlines
              ["# Config {.config}"
              ,"```"
              ,"{}"
              ,"```"
              ]
        result `shouldBe` Right defaultConfig

      it "parses JSON structure" $ do
        let result = parseMarkdown $ T.unlines
              ["# Config {.config}"
              ,"```"
              ,"{"
              ,"  \"exact_match\": true,"
              ,"  \"ignore_case\": true,"
              ,"  \"source_dirs\": [\"foo\", \"bar\"],"
              ,"  \"use_direnv\": true,"
              ,"  \"use_nix\": true"
              ,"}"
              ,"```"
              ]
        result `shouldSatisfy` \case
          Right Cfg.Config { Cfg.exact_match = Just True
                           , Cfg.ignore_case = Just True
                           , Cfg.source_dirs = ["foo", "bar"]
                           , Cfg.use_direnv = Just True
                           , Cfg.use_nix = Just True
          } -> True
          _ -> False

      describe "errors" $ do
        it "without config block" $ do
          let result = parseMarkdown $ T.unlines
                ["# Config {.config}"
                ]
          result `shouldBe` Left "Expecting config source after header"

        it "on empty JSON config block" $ do
          let result = parseMarkdown $ T.unlines
                ["# Config {.config}"
                ,"```json"
                ,"```"
                ]
          result `shouldSatisfy` match_error "not enough input"

        it "with unexpected bash config block" $ do
          let result = parseMarkdown $ T.unlines
                ["# Config {.config}"
                ,"```bash"
                ,"```"
                ]
          result `shouldBe` Left "Invalid config language: bash"

        it "on config JSON syntax error" $ do
          let result = parseMarkdown $ T.unlines
                ["# Config {.config}"
                ,"```json"
                ,"{,}"
                ,"```"
                ]
          result `shouldSatisfy` match_error "object key"

    describe "commands section" $ do
      describe "errors" $ do
        it "without source block" $ do
          let result = parseMarkdown $ T.unlines
                ["# command {.command}"
                ]
          result `shouldBe` Left "Expecting source block for command"

      it "extracts source block" $ do
        let result = parseMarkdown $ T.unlines
              ["# hello {.command .bg}"
              ,""
              ,"Command description."
              ,""
              ,"```bash"
              ,"echo Hello World"
              ,"```"
              ]
        result `shouldSatisfy` \case
          Right Cfg.Config
            { Cfg.commands = [Cmd.Command
              { Cmd.cmdName = "hello"
              , Cmd.cmdLang = "bash"
              , Cmd.cmdDesc = Just "Command description."
              , Cmd.cmdParts = [Cmd.TextPart "echo Hello World"]
              , Cmd.cmdIsBg = True
              }]
            } -> True
          _ -> False

      it "detects command by code block" $ do
        let result = parseMarkdown $ T.unlines
              ["# `hello`"
              ,"```bash"
              ,"echo Hello World"
              ,"```"
              ]
        result `shouldSatisfy` \case
          Right Cfg.Config
            { Cfg.commands = [Cmd.Command
              { Cmd.cmdName = "hello"
              , Cmd.cmdLang = "bash"
              , Cmd.cmdParts = [Cmd.TextPart "echo Hello World"]
              }]
            } -> True
          _ -> False

      xdescribe "placeholders" $ pure ()
