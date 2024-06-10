{- |
Extras for module 'Test.Tasty.Hedgehog'. Functions in this module should be
upstreamed if possible.
-}
module Test.Tasty.Hedgehog.Extra (testProperty) where

import Data.String
import Hedgehog (Property)
import Test.Tasty (TestTree)
import qualified Test.Tasty.Hedgehog as H
import Prelude

-- | Like 'Test.Tasty.Hedgehog.testProperty', but inserts correct name
testProperty :: [Char] -> Property -> TestTree
testProperty nm = H.testPropertyNamed testName propName
 where
  testName = fromString $ "prop " <> nm
  propName = fromString $ "prop_" <> nm
