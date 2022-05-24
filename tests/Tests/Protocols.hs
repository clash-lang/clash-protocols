module Tests.Protocols (tests, main) where

import           Prelude
import           Test.Tasty

import qualified Tests.Protocols.Df
import qualified Tests.Protocols.Wishbone

tests :: TestTree
tests = testGroup "Protocols"
  [ Tests.Protocols.Df.tests
  , Tests.Protocols.Wishbone.tests
  ]

main :: IO ()
main = defaultMain tests
