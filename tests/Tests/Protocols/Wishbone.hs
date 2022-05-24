module Tests.Protocols.Wishbone where


import           Clash.Prelude

import           Clash.Hedgehog.Sized.BitVector

import           Hedgehog

import           Hedgehog.Gen                   as Gen

import           Hedgehog.Range                 as Range

import           Protocols
import           Protocols.Hedgehog             (defExpectOptions, idWithModel)
import           Protocols.Wishbone
import           Test.Tasty
import           Test.Tasty.Hedgehog.Extra      (testProperty)
import           Test.Tasty.TH


idWb :: (KnownDomain dom, KnownNat addressWidth) => Circuit (Wishbone dom mode addressWidth a) (Wishbone dom mode addressWidth a)
idWb = Circuit (uncurry go)
  where
    go :: Signal dom (WishboneM2S addressWidth (BitSize a `DivRU` 8) a)
       -> Signal dom (WishboneS2M a)
       -> (Signal dom (WishboneS2M a), Signal dom (WishboneM2S addressWidth (BitSize a `DivRU` 8) a))
    go m2s s2m = (s2m, m2s)


smallInt :: Range Int
smallInt = Range.linear 0 10

genSmallInt :: Gen Int
genSmallInt = Gen.integral smallInt

genData :: Gen a -> Gen [a]
genData genA = do
  n <- genSmallInt
  Gen.list (Range.singleton n) genA

genWishboneTransfer :: (KnownNat addressWidth) => Gen a -> Gen (WishboneTransfer addressWidth a)
genWishboneTransfer genA = do
  Gen.choice [Read <$> genBitVector, Write <$> genBitVector <*> genA]


prop_id :: Property
prop_id = idWithModel defExpectOptions (genData $ genWishboneTransfer @5 genSmallInt) id (idWb @System)


tests :: TestTree
tests = $(testGroupGenerator)
