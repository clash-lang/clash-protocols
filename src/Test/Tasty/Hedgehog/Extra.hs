{-|
Extras for module 'Test.Tasty.Hedgehog'. Functions in this module should be
upstreamed if possible.
-}

module Test.Tasty.Hedgehog.Extra (testProperty) where

import Prelude
import Hedgehog (Property)
import qualified Test.Tasty.Hedgehog as H
import Test.Tasty (TestTree)

-- | Like 'Test.Tasty.Hedgehog.testProperty', but inserts correct name
testProperty :: [Char] -> Property -> TestTree
testProperty nm = H.testProperty ("prop_" <> nm)
