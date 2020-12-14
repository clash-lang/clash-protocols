module Test.Tasty.Hedgehog.Extra (testProperty) where

import Prelude
import Hedgehog (Property)
import qualified Test.Tasty.Hedgehog as H
import Test.Tasty (TestTree)

testProperty :: [Char] -> Property -> TestTree
testProperty nm = H.testProperty ("prop_" <> nm)
