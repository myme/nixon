{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Monoid law, right identity" #-}
{-# HLINT ignore "Monoid law, left identity" #-}

module Test.Nixon.Backend.Fzf where

import Data.Functor ((<&>))
import qualified Data.Text as T
import qualified Nixon.Backend as Backend
import Nixon.Backend.Fzf (fzf, fzfBackend, fzfExpect, fzfFilter, fzfProjects)
import qualified Nixon.Backend.Fzf as Fzf
import qualified Nixon.Command as Cmd
import qualified Nixon.Command.Placeholder as Cmd
import Nixon.Config.Types (defaultConfig)
import Nixon.Prelude
import Nixon.Project (Project (..))
import Nixon.Select (Candidate (Identity), Selection (..), SelectionType (Default, Edit))
import qualified Nixon.Select as Select
import System.Exit (ExitCode (..))
import Test.Hspec
import Test.Nixon.TestLib (runProc)
import Test.QuickCheck (Testable (property), chooseAny, oneof)
import Test.QuickCheck.Property (forAll)
import Turtle (d, format, (%))
import Turtle.Shell (select)

monoid_law :: (Monoid m, Eq m, Show m) => (a -> m) -> a -> Expectation
monoid_law f x = f x <> mempty `shouldBe` mempty <> f x

fzfTests :: SpecWith ()
fzfTests = do
  describe
    "FzfConfig"
    ( do
        it "is associative" $ do
          let configA = Fzf.fzfBorder
              configB = Fzf.fzfHeader "Some header"
              configC = Fzf.fzfHeight 10
          configA <> (configB <> configC) `shouldBe` (configA <> configB) <> configC

        describe "fzfBorder" $ do
          it "respects identity"
            $ monoid_law
            $ const Fzf.fzfBorder

          it "includes --border"
            $ Fzf.fzfBuildArgs Fzf.fzfBorder
            `shouldBe` ["--border"]

        describe "fzf_exact" $ do
          it "respects identity"
            $ property
            $ monoid_law Fzf.fzfExact

          it "`False` excludes --exact" $ do
            Fzf.fzfBuildArgs (Fzf.fzfExact False) `shouldBe` []

          it "`True` includes --exact" $ do
            Fzf.fzfBuildArgs (Fzf.fzfExact True) `shouldBe` ["--exact"]

        describe "fzf_ignore_case" $ do
          it "respects identity"
            $ property
            $ monoid_law Fzf.fzfIgnoreCase

          it "`False` excludes -i" $ do
            Fzf.fzfBuildArgs (Fzf.fzfIgnoreCase False) `shouldBe` []

          it "`True` includes -i" $ do
            Fzf.fzfBuildArgs (Fzf.fzfIgnoreCase True) `shouldBe` ["-i"]

        describe "fzfHeader" $ do
          it "respects identity"
            $ property
            $ monoid_law (Fzf.fzfHeader . T.pack)

          it "sets --header" $ do
            Fzf.fzfBuildArgs (Fzf.fzfHeader "<header>") `shouldBe` ["--header", "<header>"]

          it "sets --height" $ do
            Fzf.fzfBuildArgs (Fzf.fzfHeight 100) `shouldBe` ["--height", "100%"]

        describe "fzf_query" $ do
          it "respects identity"
            $ property
            $ monoid_law (Fzf.fzfQuery . T.pack)

          it "sets --query" $ do
            Fzf.fzfBuildArgs (Fzf.fzfQuery "<query>") `shouldBe` ["--query", "<query>"]

        describe "fzf_filter" $ do
          it "respects identity"
            $ property
            $ monoid_law (Fzf.fzfFilter . T.pack)

          it "sets --filter" $ do
            Fzf.fzfBuildArgs (Fzf.fzfFilter "<filter>") `shouldBe` ["--filter", "<filter>"]

        describe "fzf_preview" $ do
          it "respects identity"
            $ property
            $ monoid_law (Fzf.fzfPreview . T.pack)

          it "sets --preview" $ do
            Fzf.fzfBuildArgs (Fzf.fzfPreview "<preview>") `shouldBe` ["--preview", "<preview>"]

        describe "fzf_with_nth" $ do
          it "respects identity"
            $ let gen =
                    oneof
                      [ chooseAny <&> Fzf.FieldIndex,
                        chooseAny <&> Fzf.FieldTo,
                        chooseAny <&> Fzf.FieldFrom,
                        chooseAny <&> uncurry Fzf.FieldRange,
                        pure Fzf.AllFields
                      ]
               in property $ forAll gen $ monoid_law Fzf.fzfWithNth

          it "FieldIndex builds field"
            $ property
            $ \x ->
              Fzf.fzfBuildArgs (Fzf.fzfWithNth $ Fzf.FieldIndex x)
                `shouldBe` ["--with-nth", format d x]

          it "FieldTo builds field"
            $ property
            $ \x ->
              Fzf.fzfBuildArgs (Fzf.fzfWithNth $ Fzf.FieldTo x)
                `shouldBe` ["--with-nth", format (".." % d) x]

          it "FieldFrom builds field"
            $ property
            $ \x ->
              Fzf.fzfBuildArgs (Fzf.fzfWithNth $ Fzf.FieldFrom x)
                `shouldBe` ["--with-nth", format (d % "..") x]

          it "FieldRange builds ranges"
            $ property
            $ \(x, y) ->
              Fzf.fzfBuildArgs (Fzf.fzfWithNth $ Fzf.FieldRange x y)
                `shouldBe` ["--with-nth", format (d % ".." % d) x y]

        describe "fzf_no_sort sets" $ do
          it "sets --no_sort" $ do
            Fzf.fzfBuildArgs Fzf.fzfNoSort `shouldBe` ["--no-sort"]
    )

  describe "Fzf backend" $ do
    it "multi-select" $ do
      let candidates = map Identity ["one two three", "four five six", "seven eight nine"]
          selector = Backend.selector $ fzfBackend defaultConfig
          selectOpts = Select.defaults

      result <-
        runProc (ExitSuccess, "1\n3")
          $ Select.runSelect selector
          $ Select.select selectOpts (select candidates)

      result `shouldBe` Selection Default ["one two three", "seven eight nine"]

    it "filters fields based on selector options (words 1 & 3)" $ do
      let candidates = map Identity ["one two three", "four five six", "seven eight nine"]
          selector = Backend.selector $ fzfBackend defaultConfig
          selectOpts = Select.defaults {Select.selector_fields = Cmd.Field <$> [1, 3]}

      result <-
        runProc (ExitSuccess, "1")
          $ Select.runSelect selector
          $ Select.select selectOpts (select candidates)

      result `shouldBe` Selection Default ["one three"]

  describe
    "Fzf command"
    ( do
        it "No input returns empty selection" $ do
          result <- runProc (ExitSuccess, "") $ fzf mempty (select [])
          result `shouldBe` EmptySelection

        it "Exit code 130 gives canceled selection" $ do
          result <- runProc (ExitFailure 130, "") $ fzf mempty (select [])
          result `shouldBe` CanceledSelection

        it "Fzf matches based on line number prefix" $ do
          let candidates = map Identity ["one", "two", "three"]

          result1 <- runProc (ExitSuccess, "1") $ fzf mempty (select candidates)
          result1 `shouldBe` Selection Default ["one"]

          result2 <- runProc (ExitSuccess, "2") $ fzf mempty (select candidates)
          result2 `shouldBe` Selection Default ["two"]

          result3 <- runProc (ExitSuccess, "3") $ fzf mempty (select candidates)
          result3 `shouldBe` Selection Default ["three"]

        describe "Filter" $ do
          it "Passes result straight through" $ do
            let candidates = map Identity ["one", "two", "three"]
                opts = fzfFilter "two"

            result <- runProc (ExitSuccess, "two") $ fzf opts (select candidates)
            result `shouldBe` Selection Default ["two"]

          it "Passes multi-line output through" $ do
            let candidates = map Identity ["one", "two", "three", "oneone"]
                opts = fzfFilter "one"

            result <- runProc (ExitSuccess, "one\noneone") $ fzf opts (select candidates)
            result `shouldBe` Selection Default ["one", "oneone"]

        describe "Expect keys" $ do
          it "Parses expected keys for default selection" $ do
            let candidates = map Identity ["one", "two", "three"]
                opts = fzfExpect "alt-enter" Edit

            result <- runProc (ExitSuccess, "\n1") $ fzf opts (select candidates)
            result `shouldBe` Selection Default ["one"]

          it "Parses expected keys for alternative selection" $ do
            let candidates = map Identity ["one", "two", "three"]
                opts = fzfExpect "alt-enter" Edit

            result <- runProc (ExitSuccess, "alt-enter\n1") $ fzf opts (select candidates)
            result `shouldBe` Selection Edit ["one"]

          it "Ignores expect keys first line when using filter" $ do
            let candidates = map Identity ["one", "two", "three"]
                opts = fzfExpect "alt-enter" Edit <> fzfFilter "two"

            result <- runProc (ExitSuccess, "two") $ fzf opts (select candidates)
            result `shouldBe` Selection Default ["two"]

        describe "fzfPojects" $ do
          it "finds projects" $ do
            let project1 = Project "test-project" "/some/path" []
                projects = [project1]
            result <- runProc (ExitSuccess, "\n1") $ fzfProjects mempty Nothing projects
            result `shouldBe` Selection Default [project1]

        let project1 = Project "test-project" "/some/path" []
            command1 = Cmd.empty
            command2 = Cmd.empty
            commands = [command1, command2]

        describe "fzfPojectCommand" $ do
          it "finds project command with default selection" $ do
            result <- runProc (ExitSuccess, "\n1") $ Fzf.fzfProjectCommand mempty project1 "prompt" mempty commands
            result `shouldBe` Selection Default [command1]

          it "finds project command with edit selection" $ do
            result <- runProc (ExitSuccess, "alt-enter\n1") $ Fzf.fzfProjectCommand mempty project1 "prompt" mempty commands
            result `shouldBe` Selection Edit [command1]
    )
