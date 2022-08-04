module Tests.Protocols (tests, main) where

import Prelude
import Test.Tasty

import qualified Tests.Protocols.Df
import qualified Tests.Protocols.DfConv
import qualified Tests.Protocols.AvalonMemMap

tests :: TestTree
tests = testGroup "Protocols"
  [ Tests.Protocols.Df.tests
  , Tests.Protocols.DfConv.tests
  , Tests.Protocols.AvalonMemMap.tests
  ]

main :: IO ()
main = defaultMain tests
