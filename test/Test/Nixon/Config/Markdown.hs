{-# LANGUAGE LambdaCase #-}

module Test.Nixon.Config.Markdown where

import           Data.Text (Text)
import qualified Data.Text as T
import           Nixon.Command (CommandEnv(Env))
import qualified Nixon.Command as Cmd
import           Nixon.Config.Markdown (parseMarkdown, parseHeaderArgs)
import           Nixon.Config.Types (defaultConfig)
import qualified Nixon.Config.Types as Cfg
import           Nixon.Language (Language(Bash))
import           Test.Hspec

match_error :: Text -> Either Text b -> Bool
match_error match = either (T.isInfixOf match) (const False)

config_tests :: SpecWith ()
config_tests = describe "config section" $ do
  it "allows empty JSON object" $ do
    let result = parseMarkdown "some-file.md" $ T.unlines
          ["# Config {.config}"
          ,"```"
          ,"{}"
          ,"```"
          ]
    result `shouldBe` Right defaultConfig

  it "parses JSON structure" $ do
    let result = parseMarkdown "some-file.md" $ T.unlines
          ["# Config {.config}"
          ,"```"
          ,"{"
          ,"  \"exact_match\": true,"
          ,"  \"ignore_case\": true,"
          ,"  \"project_dirs\": [\"foo\", \"bar\"],"
          ,"  \"use_direnv\": true,"
          ,"  \"use_nix\": true"
          ,"}"
          ,"```"
          ]
    result `shouldSatisfy` \case
      Right Cfg.Config { Cfg.exact_match = Just True
                       , Cfg.ignore_case = Just True
                       , Cfg.project_dirs = ["foo", "bar"]
                       , Cfg.use_direnv = Just True
                       , Cfg.use_nix = Just True
      } -> True
      _ -> False

  describe "errors" $ do
    it "without config block" $ do
      let result = parseMarkdown "some-file.md" $ T.unlines
            ["# Config {.config}"
            ]
      result `shouldBe` Left "Expecting config source after header"

    it "on empty JSON config block" $ do
      let result = parseMarkdown "some-file.md" $ T.unlines
            ["# Config {.config}"
            ,"```json"
            ,"```"
            ]
      result `shouldSatisfy` match_error "not enough input"

    it "with unexpected bash config block" $ do
      let result = parseMarkdown "some-file.md" $ T.unlines
            ["# Config {.config}"
            ,"```bash"
            ,"```"
            ]
      result `shouldBe` Left "Invalid config language: bash"

    it "on config JSON syntax error" $ do
      let result = parseMarkdown "some-file.md" $ T.unlines
            ["# Config {.config}"
            ,"```json"
            ,"{,}"
            ,"```"
            ]
      result `shouldSatisfy` match_error "object key"

command_tests :: SpecWith ()
command_tests = describe "commands section" $ do
  describe "errors" $ do
    it "without source block" $ do
      let result = parseMarkdown "some-file.md" $ T.unlines
            ["# command {.command}"
            ]
      result `shouldBe` Left "Expecting source block for command"

    it "simple command name with arg" $ do
      let result = parseMarkdown "some-file.md" $ T.unlines
            ["# hello {.command}"
            ,"```bash"
            ,"echo Hello World"
            ,"```"
            ]
      result `shouldSatisfy` \case
        Right Cfg.Config
          { Cfg.commands =
            [Cmd.Command { Cmd.cmdName = "hello" }] } -> True
        _ -> False

    it "simple command name with ticks" $ do
      let result = parseMarkdown "some-file.md" $ T.unlines
            ["# `hello`"
            ,"```bash"
            ,"echo Hello World"
            ,"```"
            ]
      result `shouldSatisfy` \case
        Right Cfg.Config
          { Cfg.commands =
            [Cmd.Command { Cmd.cmdName = "hello" }] } -> True
        _ -> False

  it "command name is first word" $ do
    let result = parseMarkdown "some-file.md" $ T.unlines
          ["# `hello ${foo} ${bar}`"
          ,"```bash"
          ,"echo Hello World"
          ,"```"
          ]
    result `shouldSatisfy` \case
      Right Cfg.Config
        { Cfg.commands =
          [Cmd.Command { Cmd.cmdName = "hello" }] } -> True
      _ -> False

  it "extracts source block" $ do
    let result = parseMarkdown "some-file.md" $ T.unlines
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
          , Cmd.cmdLang = Bash
          , Cmd.cmdDesc = Just "Command description."
          , Cmd.cmdSource = "echo Hello World\n"
          , Cmd.cmdEnv = []
          , Cmd.cmdIsBg = True
          }]
        } -> True
      _ -> False

  it "detects command by code block" $ do
    let result = parseMarkdown "some-file.md" $ T.unlines
          ["# `hello`"
          ,"```bash"
          ,"echo Hello World"
          ,"```"
          ]
    result `shouldSatisfy` \case
      Right Cfg.Config
        { Cfg.commands = [Cmd.Command
          { Cmd.cmdName = "hello"
          , Cmd.cmdLang = Bash
          , Cmd.cmdSource = "echo Hello World\n"
          , Cmd.cmdEnv = []
          , Cmd.cmdIsBg = False
          }]
        } -> True
      _ -> False

  it "detects json output format" $ do
    let result = parseMarkdown "some-file.md" $ T.unlines
          ["# `hello` {.json}"
          ,"```bash"
          ,"echo Hello World"
          ,"```"
          ]
    result `shouldSatisfy` \case
      Right Cfg.Config
        { Cfg.commands = [Cmd.Command
          { Cmd.cmdName = "hello"
          , Cmd.cmdOutput = Cmd.JSON
          }]
        } -> True
      _ -> False

  it "detects project type" $ do
    let result = parseMarkdown "some-file.md" $ T.unlines
          ["# `hello` {type=\"git\"}"
          ,"```bash"
          ,"echo Hello World"
          ,"```"
          ]
    result `shouldSatisfy` \case
      Right Cfg.Config
        { Cfg.commands = [Cmd.Command
          { Cmd.cmdName = "hello"
          , Cmd.cmdProjectTypes = ["git"]
          }]
        } -> True
      _ -> False

  it "detects background commands by &" $ do
    let result = parseMarkdown "some-file.md" $ T.unlines
          ["# `hello &`"
          ,"```bash"
          ,"echo Hello World"
          ,"```"
          ]
    result `shouldSatisfy` \case
      Right Cfg.Config
        { Cfg.commands = [Cmd.Command
          { Cmd.cmdName = "hello"
          , Cmd.cmdIsBg = True
          }]
        } -> True
      _ -> False

  it "supports alternate header format" $ do
    let result = parseMarkdown "some-file.md" $ T.unlines
          ["`hello`"
          ,"======="
          ,"```bash"
          ,"echo Hello World"
          ,"```"
          ]
    result `shouldSatisfy` \case
      Right Cfg.Config { Cfg.commands = [Cmd.Command { Cmd.cmdName = "hello" }] } -> True
      _ -> False

  describe "extracts environment placeholders" $ do
    it "fails" $ do
      let result = parseMarkdown "some-file.md" $ T.unlines
            ["# `hello ${arg} ${another-arg} &`"
            ,"```bash"
            ,"echo Hello \"$arg\" \"$another_arg\""
            ,"```"
            ]
      result `shouldSatisfy` \case
        Right Cfg.Config
          { Cfg.commands = [Cmd.Command
            { Cmd.cmdName = "hello"
            , Cmd.cmdIsBg = True
            , Cmd.cmdEnv = [("arg", Env "arg"), ("another_arg", Env "another-arg")]
            }]
          } -> True
        _ -> False

parse_header_tests :: SpecWith ()
parse_header_tests = describe "parseHeaderArgs" $ do
  it "extracts name" $ do
    parseHeaderArgs "some header" `shouldBe` ("some header", [], [])

  it "extracts name, arg and kwarg" $ do
    parseHeaderArgs "some header {.some-arg some-kw=\"value\"}" `shouldBe`
      ("some header", ["some-arg"], [("some-kw", "value")])

  it "extracts .bg" $ do
    parseHeaderArgs "{.bg}" `shouldBe` ("", ["bg"], [])

  it "extracts .config" $ do
    parseHeaderArgs "{.config}" `shouldBe` ("", ["config"], [])

  it "extracts .command" $ do
    parseHeaderArgs "{.command}" `shouldBe` ("", ["command"], [])

  it "extracts .json" $ do
    parseHeaderArgs "{.json}" `shouldBe` ("", ["json"], [])

  it "extracts type" $ do
    parseHeaderArgs "{type=git}" `shouldBe` ("", [], [("type", "git")])

  it "extracts type with quotes" $ do
    parseHeaderArgs "{type=\"git\"}" `shouldBe` ("", [], [("type", "git")])

  it "mix args and kwargs" $ do
    parseHeaderArgs "{.command type=\"git\"}" `shouldBe` ("", ["command"], [("type", "git")])

markdown_tests :: SpecWith ()
markdown_tests = do
  describe "parseMarkdown" $ do
    config_tests
    command_tests
    parse_header_tests
