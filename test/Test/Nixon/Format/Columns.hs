module Test.Nixon.Format.Columns where

import Nixon.Format (formatColumns, parseColumns)
import Nixon.Prelude
import Test.Hspec

column_tests :: SpecWith ()
column_tests = do
  formatColumns_tests
  it "parses columns (empty input)" $ do
    parseColumns False [] `shouldBe` []
    parseColumns True [] `shouldBe` []

  it "parses columns (titles only)" $ do
    let input = ["NAME               UUID                                  TYPE      DEVICE"]
    parseColumns False input `shouldBe` [["NAME", "UUID", "TYPE", "DEVICE"]]
    parseColumns True input `shouldBe` []

  it "parses columns" $ do
    let input =
          [ "NAME               UUID                                  TYPE      DEVICE",
            "My Wifi            845b3837-c78e-44f1-a752-06ecd496599c  wifi      wlp9s0",
            "br-7defdaf327de    1b9a3d7c-d856-498f-ac12-4d79647f116f  bridge    br-7defdaf327de",
            "lo                 ae505c7d-8596-41b2-9329-c3d31f4c60ef  loopback  lo"
          ]
    parseColumns True input
      `shouldBe` [ ["My Wifi", "845b3837-c78e-44f1-a752-06ecd496599c", "wifi", "wlp9s0"],
                   ["br-7defdaf327de", "1b9a3d7c-d856-498f-ac12-4d79647f116f", "bridge", "br-7defdaf327de"],
                   ["lo", "ae505c7d-8596-41b2-9329-c3d31f4c60ef", "loopback", "lo"]
                 ]

  it "parses columns (no headers)" $ do
    let input =
          [ "br-7defdaf327de    1b9a3d7c-d856-498f-ac12-4d79647f116f  bridge    br-7defdaf327de",
            "My Wifi            845b3837-c78e-44f1-a752-06ecd496599c  wifi      wlp9s0",
            "lo                 ae505c7d-8596-41b2-9329-c3d31f4c60ef  loopback  lo"
          ]
    parseColumns False input
      `shouldBe` [ ["br-7defdaf327de", "1b9a3d7c-d856-498f-ac12-4d79647f116f", "bridge", "br-7defdaf327de"],
                   ["My Wifi", "845b3837-c78e-44f1-a752-06ecd496599c", "wifi", "wlp9s0"],
                   ["lo", "ae505c7d-8596-41b2-9329-c3d31f4c60ef", "loopback", "lo"]
                 ]

formatColumns_tests :: SpecWith ()
formatColumns_tests = do
  -- Regression: `cols` (no header) must keep every row, with each title
  -- aligned to its own value. Previously the first row was dropped and the
  -- remaining titles were shifted relative to their values.
  it "keeps all rows and aligns titles to values (no header)" $ do
    let input =
          [ "/home/myme/code/myme/nixon         99dad83 [main]",
            "/home/myme/code/myme/nixon-another 99dad83 [nixon-another]",
            "/home/myme/code/myme/nixon-test    99dad83 [nixon-test]"
          ]
    formatColumns False [1] input
      `shouldBe` [ ("/home/myme/code/myme/nixon         99dad83 [main]", "/home/myme/code/myme/nixon"),
                   ("/home/myme/code/myme/nixon-another 99dad83 [nixon-another]", "/home/myme/code/myme/nixon-another"),
                   ("/home/myme/code/myme/nixon-test    99dad83 [nixon-test]", "/home/myme/code/myme/nixon-test")
                 ]

  it "drops the header row from both titles and values (with header)" $ do
    let input =
          [ "NAME               UUID                                  TYPE      DEVICE",
            "My Wifi            845b3837-c78e-44f1-a752-06ecd496599c  wifi      wlp9s0",
            "lo                 ae505c7d-8596-41b2-9329-c3d31f4c60ef  loopback  lo"
          ]
    formatColumns True [1] input
      `shouldBe` [ ("My Wifi            845b3837-c78e-44f1-a752-06ecd496599c  wifi      wlp9s0", "My Wifi"),
                   ("lo                 ae505c7d-8596-41b2-9329-c3d31f4c60ef  loopback  lo", "lo")
                 ]
