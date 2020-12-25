module Tests.Protocols (tests, main) where

import Prelude
import Test.Tasty

import qualified Tests.Protocols.Df

tests :: TestTree
tests = testGroup "Protocols"
  [ Tests.Protocols.Df.tests
  ]

main :: IO ()
main = defaultMain tests
