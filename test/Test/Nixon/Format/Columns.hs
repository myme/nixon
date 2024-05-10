module Test.Nixon.Format.Columns where

import qualified Data.Text as T
import Nixon.Format (parseColumns)
import Nixon.Prelude
import Test.Hspec

column_tests :: SpecWith ()
column_tests = do
  it "parses columns (empty input)" $ do
    parseColumns "" `shouldBe` []

  it "parses columns (titles only)" $ do
    let input = "NAME               UUID                                  TYPE      DEVICE"
    parseColumns input `shouldBe` []

  it "parses columns" $ do
    let input =
          T.unlines
            [ "NAME               UUID                                  TYPE      DEVICE",
              "My Wifi            845b3837-c78e-44f1-a752-06ecd496599c  wifi      wlp9s0",
              "br-7defdaf327de    1b9a3d7c-d856-498f-ac12-4d79647f116f  bridge    br-7defdaf327de",
              "lo                 ae505c7d-8596-41b2-9329-c3d31f4c60ef  loopback  lo"
            ]
    parseColumns input
      `shouldBe` [ ["My Wifi", "845b3837-c78e-44f1-a752-06ecd496599c", "wifi", "wlp9s0"],
                   ["br-7defdaf327de", "1b9a3d7c-d856-498f-ac12-4d79647f116f", "bridge", "br-7defdaf327de"],
                   ["lo", "ae505c7d-8596-41b2-9329-c3d31f4c60ef", "loopback", "lo"]
                 ]
