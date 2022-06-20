module Tests.Protocols (tests, main) where

import Prelude
import Test.Tasty

import qualified Tests.Protocols.Df
import qualified Tests.Protocols.Fifo

tests :: TestTree
tests = testGroup "Protocols"
  [ Tests.Protocols.Df.tests
  , Tests.Protocols.Fifo.tests
  ]

main :: IO ()
main = defaultMain tests
