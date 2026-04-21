module Tests.Protocols (tests, main) where

import Prelude

import Test.Tasty
import Tests.Protocols.Df.Extra qualified

tests :: TestTree
tests =
  testGroup
    "Protocols"
    [ Tests.Protocols.Df.Extra.tests
    ]

main :: IO ()
main = defaultMain tests
