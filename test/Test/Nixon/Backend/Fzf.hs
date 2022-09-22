{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Monoid law, right identity" #-}
{-# HLINT ignore "Monoid law, left identity" #-}

module Test.Nixon.Backend.Fzf where

import Data.Functor ((<&>))
import qualified Data.Text as T
import qualified Nixon.Backend.Fzf as Fzf
import Test.Hspec
import Test.QuickCheck (Testable (property), chooseAny, oneof)
import Test.QuickCheck.Property (forAll)
import Turtle (d, format, (%))

monoid_law :: (Monoid m, Eq m, Show m) => (a -> m) -> a -> Expectation
monoid_law f x = f x <> mempty `shouldBe` mempty <> f x

fzf :: SpecWith ()
fzf = do
  describe "FzfConfig" $ do
    it "is associative" $ do
      let configA = Fzf.fzfBorder
          configB = Fzf.fzfHeader "Some header"
          configC = Fzf.fzfHeight 10
      configA <> (configB <> configC) `shouldBe` (configA <> configB) <> configC

    describe "fzfBorder" $ do
      it "respects identity" $
        monoid_law $ const Fzf.fzfBorder

      it "includes --border" $
        Fzf.fzfBuildArgs Fzf.fzfBorder `shouldBe` ["--border"]

    describe "fzf_exact" $ do
      it "respects identity" $
        property $ monoid_law Fzf.fzfExact

      it "`False` excludes --exact" $ do
        Fzf.fzfBuildArgs (Fzf.fzfExact False) `shouldBe` []

      it "`True` includes --exact" $ do
        Fzf.fzfBuildArgs (Fzf.fzfExact True) `shouldBe` ["--exact"]

    describe "fzf_ignore_case" $ do
      it "respects identity" $
        property $ monoid_law Fzf.fzfIgnoreCase

      it "`False` excludes -i" $ do
        Fzf.fzfBuildArgs (Fzf.fzfIgnoreCase False) `shouldBe` []

      it "`True` includes -i" $ do
        Fzf.fzfBuildArgs (Fzf.fzfIgnoreCase True) `shouldBe` ["-i"]

    describe "fzfHeader" $ do
      it "respects identity" $
        property $ monoid_law (Fzf.fzfHeader . T.pack)

      it "sets --header" $ do
        Fzf.fzfBuildArgs (Fzf.fzfHeader "<header>") `shouldBe` ["--header", "<header>"]

      it "sets --height" $ do
        Fzf.fzfBuildArgs (Fzf.fzfHeight 100) `shouldBe` ["--height", "100%"]

    describe "fzf_query" $ do
      it "respects identity" $
        property $ monoid_law (Fzf.fzfQuery . T.pack)

      it "sets --query" $ do
        Fzf.fzfBuildArgs (Fzf.fzfQuery "<query>") `shouldBe` ["--query", "<query>"]

    describe "fzf_filter" $ do
      it "respects identity" $
        property $ monoid_law (Fzf.fzfFilter . T.pack)

      it "sets --filter" $ do
        Fzf.fzfBuildArgs (Fzf.fzfFilter "<filter>") `shouldBe` ["--filter", "<filter>"]

    describe "fzf_preview" $ do
      it "respects identity" $
        property $ monoid_law (Fzf.fzfPreview . T.pack)

      it "sets --preview" $ do
        Fzf.fzfBuildArgs (Fzf.fzfPreview "<preview>") `shouldBe` ["--preview", "<preview>"]

    describe "fzf_with_nth" $ do
      it "respects identity" $
        let gen =
              oneof
                [ chooseAny <&> Fzf.FieldIndex,
                  chooseAny <&> Fzf.FieldTo,
                  chooseAny <&> Fzf.FieldFrom,
                  chooseAny <&> uncurry Fzf.FieldRange,
                  pure Fzf.AllFields
                ]
         in property $ forAll gen $ monoid_law Fzf.fzfWithNth

      it "FieldIndex builds field" $
        property $ \x ->
          Fzf.fzfBuildArgs (Fzf.fzfWithNth $ Fzf.FieldIndex x)
            `shouldBe` ["--with-nth", format d x]

      it "FieldTo builds field" $
        property $ \x ->
          Fzf.fzfBuildArgs (Fzf.fzfWithNth $ Fzf.FieldTo x)
            `shouldBe` ["--with-nth", format (".." % d) x]

      it "FieldFrom builds field" $
        property $ \x ->
          Fzf.fzfBuildArgs (Fzf.fzfWithNth $ Fzf.FieldFrom x)
            `shouldBe` ["--with-nth", format (d % "..") x]

      it "FieldRange builds ranges" $
        property $ \(x, y) ->
          Fzf.fzfBuildArgs (Fzf.fzfWithNth $ Fzf.FieldRange x y)
            `shouldBe` ["--with-nth", format (d % ".." % d) x y]

    describe "fzf_no_sort sets" $ do
      it "sets --no_sort" $ do
        Fzf.fzfBuildArgs Fzf.fzfNoSort `shouldBe` ["--no-sort"]
