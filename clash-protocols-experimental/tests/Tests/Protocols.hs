module Tests.Protocols (tests, main) where

import Prelude

import Test.Tasty
import Tests.Protocols.BiDf qualified
import Tests.Protocols.Df.Extra qualified

tests :: TestTree
tests =
  testGroup
    "Protocols"
    [ Tests.Protocols.BiDf.tests
    , Tests.Protocols.Df.Extra.tests
    ]

main :: IO ()
main = defaultMain tests
