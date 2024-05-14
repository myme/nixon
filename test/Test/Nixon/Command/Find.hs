module Test.Nixon.Command.Find where

import qualified Data.Text as T
import qualified Nixon.Command as Cmd
import Nixon.Command.Find (findProjectCommands)
import Nixon.Config.Markdown (parseMarkdown)
import Nixon.Config.Types (defaultConfig)
import qualified Nixon.Config.Types as Conf
import Nixon.Prelude
import Nixon.Project (Project (projectTypes), ProjectMarker (ProjectFunc), from_path, proj)
import Nixon.Types (runNixon)
import Test.Hspec
import Test.Nixon.TestLib

findTests :: SpecWith ()
findTests = do
  it "fetch empty commands" $ do
    cmds <- withTmpDir [] $ \path -> do
      let cfg = defaultConfig
          project = from_path path
      runNixon cfg $ findProjectCommands project
    cmds `shouldBe` []

  it "fetch markdown commands" $ do
    let cmd = Cmd.empty {Cmd.cmdName = "foo"}
    cmds <- withTmpDir [] $ \path -> do
      let cfg = Conf.defaultConfig {Conf.commands = [cmd]}
          project = from_path path
      runNixon cfg $ findProjectCommands project
    cmds `shouldBe` [cmd]

  it "filters away missing project types" $ do
    let cmd = Cmd.empty {Cmd.cmdName = "foo", Cmd.cmdProjectTypes = ["bar"]}
    cmds <- withTmpDir [] $ \path -> do
      let cfg = Conf.defaultConfig {Conf.commands = [cmd]}
          project = from_path path
      runNixon cfg $ findProjectCommands project
    cmds `shouldBe` []

  it "matches project type" $ do
    let cmd = Cmd.empty {Cmd.cmdName = "foo", Cmd.cmdProjectTypes = ["bar"]}
    cmds <- withTmpDir [] $ \path -> do
      let projectTypes = [proj "bar" [ProjectFunc $ const $ pure True] "has a bar"]
      let cfg =
            Conf.defaultConfig
              { Conf.commands = [cmd],
                Conf.project_types = projectTypes
              }
          project = (from_path path) {projectTypes = projectTypes}
      runNixon cfg $ findProjectCommands project
    cmds `shouldBe` [cmd]

  it "filters away command from markdown" $ do
    let parsed =
          parseMarkdown "some-file.md"
            $ T.unlines
              [ "# `hello` {type=\"git\"}",
                "```bash",
                "echo Hello World",
                "```"
              ]
    case parsed of
      Left e -> expectationFailure $ show e
      Right cfg -> do
        cmds <- withTmpDir [] $ \path -> do
          let project = from_path path
          runNixon cfg $ findProjectCommands project
        cmds `shouldBe` []
