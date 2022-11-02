module Test.Nixon.Config.Markdown where

import Control.Arrow ((&&&))
import Data.Either (isLeft)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Nixon.Command as Cmd
import Nixon.Command.Placeholder (Placeholder (..), PlaceholderType (..))
import Nixon.Config.Markdown (parseCommandName, parseHeaderArgs, parseMarkdown)
import Nixon.Config.Types (LogLevel (LogWarning), defaultConfig)
import qualified Nixon.Config.Types as Cfg
import Nixon.Language (Language (Bash))
import Test.Hspec

match_error :: Text -> Either Text b -> Bool
match_error match = either (T.isInfixOf match) (const False)

config_tests :: SpecWith ()
config_tests = describe "config section" $ do
  it "allows empty JSON object" $
    let result =
          parseMarkdown "some-file.md" $
            T.unlines
              [ "# Config {.config}",
                "```",
                "{}",
                "```"
              ]
     in result `shouldBe` Right defaultConfig

  it "parses JSON structure" $
    let result =
          parseMarkdown "some-file.md" $
            T.unlines
              [ "# Config {.config}",
                "```",
                "{",
                "  \"exact_match\": true,",
                "  \"ignore_case\": true,",
                "  \"project_dirs\": [\"foo\", \"bar\"],",
                "  \"use_direnv\": true,",
                "  \"use_nix\": true",
                "}",
                "```"
              ]
     in result
          `shouldBe` Right
            Cfg.Config
              { Cfg.backend = Nothing,
                Cfg.exact_match = Just True,
                Cfg.ignore_case = Just True,
                Cfg.force_tty = Nothing,
                Cfg.project_dirs = ["foo", "bar"],
                Cfg.project_types = [],
                Cfg.commands = [],
                Cfg.use_direnv = Just True,
                Cfg.use_nix = Just True,
                Cfg.terminal = Nothing,
                Cfg.loglevel = Just LogWarning
              }

  describe "errors" $ do
    it "without config block" $
      let result =
            parseMarkdown "some-file.md" $
              T.unlines
                [ "# Config {.config}"
                ]
       in result `shouldBe` Left "Expecting config source after header"

    it "on empty JSON config block" $
      let result =
            parseMarkdown "some-file.md" $
              T.unlines
                [ "# Config {.config}",
                  "```json",
                  "```"
                ]
       in result `shouldSatisfy` match_error "not enough input"

    it "with unexpected bash config block" $
      let result =
            parseMarkdown "some-file.md" $
              T.unlines
                [ "# Config {.config}",
                  "```bash",
                  "```"
                ]
       in result `shouldBe` Left "Invalid config language: bash"

    it "on config JSON syntax error" $
      let result =
            parseMarkdown "some-file.md" $
              T.unlines
                [ "# Config {.config}",
                  "```json",
                  "{,}",
                  "```"
                ]
       in result `shouldSatisfy` match_error "object key"

command_tests :: SpecWith ()
command_tests = describe "commands section" $ do
  describe "errors" $ do
    it "without source block" $
      let result =
            parseMarkdown "some-file.md" $
              T.unlines
                [ "# command {.command}"
                ]
       in result `shouldBe` Left "Expecting source block for command"

    it "simple command name with arg" $
      let result =
            parseMarkdown "some-file.md" $
              T.unlines
                [ "# hello {.command}",
                  "```bash",
                  "echo Hello World",
                  "```"
                ]
          selector = fmap Cmd.cmdName . Cfg.commands
       in selector <$> result `shouldBe` Right ["hello"]

    it "simple command name with ticks" $
      let result =
            parseMarkdown "some-file.md" $
              T.unlines
                [ "# `hello`",
                  "```bash",
                  "echo Hello World",
                  "```"
                ]
          selector = fmap (Cmd.cmdName &&& Cmd.cmdIsHidden) . Cfg.commands
       in selector <$> result `shouldBe` Right [("hello", False)]

    it "hidden command name with leading underscore" $
      let result =
            parseMarkdown "some-file.md" $
              T.unlines
                [ "# `_hidden`",
                  "```bash",
                  "echo Hello World",
                  "```"
                ]
          selector = fmap (Cmd.cmdName &&& Cmd.cmdIsHidden) . Cfg.commands
       in selector <$> result `shouldBe` Right [("_hidden", True)]

  it "command name is first word" $
    let result =
          parseMarkdown "some-file.md" $
            T.unlines
              [ "# `hello ${foo} ${bar}`",
                "```bash",
                "echo Hello World",
                "```"
              ]
        selector = fmap Cmd.cmdName . Cfg.commands
     in selector <$> result `shouldBe` Right ["hello"]

  it "extracts source block" $
    let result =
          parseMarkdown "some-file.md" $
            T.unlines
              [ "# hello {.command .bg}",
                "",
                "Command description.",
                "",
                "```bash",
                "echo Hello World",
                "```"
              ]
        selector
          ( Cmd.Command
              { Cmd.cmdName = name,
                Cmd.cmdLang = lang,
                Cmd.cmdDesc = desc,
                Cmd.cmdSource = source,
                Cmd.cmdPlaceholders = env,
                Cmd.cmdIsBg = isBg
              }
            ) = (name, lang, desc, source, env, isBg)
     in fmap selector . Cfg.commands <$> result
          `shouldBe` Right
            [ ("hello", Bash, Just "Command description.", "echo Hello World\n", [], True)
            ]

  it "detects command by code block" $
    let result =
          parseMarkdown "some-file.md" $
            T.unlines
              [ "# `hello`",
                "```bash",
                "echo Hello World",
                "```"
              ]
        selector
          ( Cmd.Command
              { Cmd.cmdName = name,
                Cmd.cmdLang = lang,
                Cmd.cmdSource = source,
                Cmd.cmdPlaceholders = env,
                Cmd.cmdIsBg = isBg
              }
            ) = (name, lang, source, env, isBg)
     in fmap selector . Cfg.commands <$> result
          `shouldBe` Right
            [ ("hello", Bash, "echo Hello World\n", [], False)
            ]

  it "detects json output format" $
    let result =
          parseMarkdown "some-file.md" $
            T.unlines
              [ "# `hello` {.json}",
                "```bash",
                "echo Hello World",
                "```"
              ]
        selector = fmap (Cmd.cmdName &&& Cmd.cmdOutput) . Cfg.commands
     in selector <$> result `shouldBe` Right [("hello", Cmd.JSON)]

  it "detects project type" $
    let result =
          parseMarkdown "some-file.md" $
            T.unlines
              [ "# `hello` {type=\"git\"}",
                "```bash",
                "echo Hello World",
                "```"
              ]
        selector = fmap (Cmd.cmdName &&& Cmd.cmdProjectTypes) . Cfg.commands
     in selector <$> result `shouldBe` Right [("hello", ["git"])]

  it "detects background commands by &" $
    let result =
          parseMarkdown "some-file.md" $
            T.unlines
              [ "# `hello &`",
                "```bash",
                "echo Hello World",
                "```"
              ]
        selector = fmap (Cmd.cmdName &&& Cmd.cmdIsBg) . Cfg.commands
     in selector <$> result `shouldBe` Right [("hello", True)]

  it "supports alternate header format" $
    let result =
          parseMarkdown "some-file.md" $
            T.unlines
              [ "`hello`",
                "=======",
                "```bash",
                "echo Hello World",
                "```"
              ]
        selector = fmap Cmd.cmdName . Cfg.commands
     in selector <$> result `shouldBe` Right ["hello"]

  describe "extracts environment placeholders" $ do
    it "fails" $
      let result =
            parseMarkdown "some-file.md" $
              T.unlines
                [ "# `hello ${arg} ${another-arg} &`",
                  "```bash",
                  "echo Hello \"$arg\" \"$another_arg\"",
                  "```"
                ]
          selector
            ( Cmd.Command
                { Cmd.cmdName = name,
                  Cmd.cmdIsBg = isBg,
                  Cmd.cmdPlaceholders = placeholders
                }
              ) = (name, isBg, placeholders)
       in fmap selector . Cfg.commands <$> result
            `shouldBe` Right
              [ ( "hello",
                  True,
                  [Placeholder Arg "arg" False, Placeholder Arg "another-arg" False]
                )
              ]

parse_header_tests :: SpecWith ()
parse_header_tests = describe "parseHeaderArgs" $ do
  it "extracts name" $
    parseHeaderArgs "some header" `shouldBe` ("some header", [], [])

  it "extracts name, arg and kwarg" $
    parseHeaderArgs "some header {.some-arg some-kw=\"value\"}"
      `shouldBe` ("some header", ["some-arg"], [("some-kw", "value")])

  it "extracts .bg" $
    parseHeaderArgs "{.bg}" `shouldBe` ("", ["bg"], [])

  it "extracts .config" $
    parseHeaderArgs "{.config}" `shouldBe` ("", ["config"], [])

  it "extracts .command" $
    parseHeaderArgs "{.command}" `shouldBe` ("", ["command"], [])

  it "extracts .json" $
    parseHeaderArgs "{.json}" `shouldBe` ("", ["json"], [])

  it "extracts type" $
    parseHeaderArgs "{type=git}" `shouldBe` ("", [], [("type", "git")])

  it "extracts type with quotes" $
    parseHeaderArgs "{type=\"git\"}" `shouldBe` ("", [], [("type", "git")])

  it "mix args and kwargs" $
    parseHeaderArgs "{.command type=\"git\"}" `shouldBe` ("", ["command"], [("type", "git")])

parse_command_name_tests :: SpecWith ()
parse_command_name_tests = describe "parseCommandName" $ do
  it "parses text part" $ do
    parseCommandName "echo 'foo bar baz'"
      `shouldBe` Right ("echo", [])

  it "parses leading spaces" $ do
    parseCommandName "   echo 'foo bar baz'"
      `shouldBe` Right ("echo", [])

  it "parses arg part" $ do
    parseCommandName "cat ${arg}"
      `shouldBe` Right ("cat", [Placeholder Arg "arg" False])

  it "parses stdin arg part" $ do
    parseCommandName "cat <{arg}"
      `shouldBe` Right ("cat", [Placeholder Stdin "arg" False])

  it "parses envvar arg part" $ do
    parseCommandName "cat ={arg}"
      `shouldBe` Right ("cat", [Placeholder EnvVar "arg" False])

  it "parses arg modifiers" $ do
    parseCommandName "cat ${arg:m}"
      `shouldBe` Right ("cat", [Placeholder Arg "arg" True])

  it "parses stdin arg modifiers" $ do
    parseCommandName "cat <{arg:m}"
      `shouldBe` Right ("cat", [Placeholder Stdin "arg" True])

  it "parses text and placeholder part" $ do
    parseCommandName "cat \"${arg}\""
      `shouldBe` Right ("cat", [Placeholder Arg "arg" False])

  it "replaces '-' with '_' in $name" $ do
    parseCommandName "cat \"${some-arg}\""
      `shouldBe` Right ("cat", [Placeholder Arg "some-arg" False])

  it "allows use of $ not matching '${'" $ do
    parseCommandName "echo $SOME_VAR" `shouldBe` Right ("echo", [])

  it "allows use of < not matching '<{'" $ do
    parseCommandName "echo <SOME_VAR" `shouldBe` Right ("echo", [])

  it "fails on unterminated arg" $ do
    parseCommandName "cat \"${arg\"" `shouldSatisfy` isLeft

markdown_tests :: SpecWith ()
markdown_tests = do
  describe "parseMarkdown" $ do
    config_tests
    command_tests
    parse_header_tests
    parse_command_name_tests
