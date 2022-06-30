{-# LANGUAGE FlexibleContexts   #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE RecordWildCards    #-}
module Tests.Protocols.Wishbone where


import           Clash.Prelude                  hiding ((&&))

import           Clash.Hedgehog.Sized.BitVector

import           Hedgehog

import           Hedgehog.Gen                   as Gen

import           Hedgehog.Range                 as Range

import           Protocols
import           Protocols.Hedgehog             (defExpectOptions)
import           Protocols.Wishbone
import           Protocols.Wishbone.Hedgehog

-- tasty
import           Control.DeepSeq                (NFData)
import           Data.Either                    (isLeft)
import           Test.Tasty
import           Test.Tasty.HUnit
import           Test.Tasty.Hedgehog            (HedgehogTestLimit (HedgehogTestLimit))
import           Test.Tasty.Hedgehog.Extra      (testProperty)
import           Test.Tasty.TH                  (testGroupGenerator)

smallInt :: Range Int
smallInt = Range.linear 0 10

genSmallInt :: Gen Int
genSmallInt = Gen.integral smallInt

genData :: Gen a -> Gen [a]
genData genA = do
  n <- genSmallInt
  Gen.list (Range.singleton n) genA

genWishboneTransfer :: (KnownNat addressWidth) => Gen a -> Gen (WishboneMasterRequest addressWidth a)
genWishboneTransfer genA = do
  Gen.choice [
      Read <$> genBitVector, Write <$> genBitVector <*> genA]

transfersToSignalsStandard
  :: forall addressWidth a . (KnownNat addressWidth, KnownNat (BitSize a), NFDataX a)
  => [WishboneMasterRequest addressWidth a]
  -> [WishboneM2S addressWidth (BitSize a `DivRU` 8) a]
transfersToSignalsStandard = Prelude.concatMap $ \case
   Read bv -> [ (wishboneM2S @addressWidth @a) { strobe = False, busCycle = False }
              , (wishboneM2S @addressWidth @a) { strobe = True, busCycle = True, addr = bv, writeEnable = False }]
   Write bv a -> [ wishboneM2S
                 , (wishboneM2S @addressWidth @a) { strobe = True, busCycle = True, addr = bv, writeEnable = True, writeData = a }]


idWriteWbSt :: forall a dom selWidth. (BitPack a, NFDataX a) => Circuit (Wishbone dom 'Standard selWidth a) ()
idWriteWbSt = Circuit go
  where
    go (m2s, ()) = (reply <$> m2s, ())

    reply WishboneM2S{..}
      | busCycle && strobe && writeEnable = (wishboneS2M @a) { acknowledge = True, readData = writeData }
      | busCycle && strobe                = wishboneS2M { acknowledge = True }
      | otherwise                         = wishboneS2M


prop_idWriteSt :: Property
prop_idWriteSt =  validateStallingStandardCircuit @System defExpectOptions genDat idWriteWbSt
  where
    genDat = genData (genWishboneTransfer @10 genSmallInt)


idWriteStModel :: (NFData a, Eq a, ShowX a) => WishboneMasterRequest addressWidth a -> WishboneS2M a -> () -> Either String ()
idWriteStModel (Read _)    s@WishboneS2M{..} ()
  | acknowledge && isLeft (hasX readData) = Right ()
  | otherwise                             = Left $ "Read should have been acknowledged with no DAT " <> showX s
idWriteStModel (Write _ a) s@WishboneS2M{..} ()
  | acknowledge && hasX readData == Right a = Right ()
  | otherwise                               = Left $ "Write should have been acknowledged with write-data as DAT " <> showX s


prop_idWriteSt_model :: Property
prop_idWriteSt_model = wishbonePropWithModel @System idWriteStModel idWriteWbSt (genData $ genWishboneTransfer @10 genSmallInt) ()


case_read_stall_0 :: Assertion
case_read_stall_0 = do
  validateStallingStandardCircuitInner @System @Int @10 defExpectOptions [Read 0, Read 0] [0, 0] idWriteWbSt @=? Right ()

case_model_read_stall_1 :: Assertion
case_model_read_stall_1 = do
  wishbonePropWithModelInner @System @Int @10 idWriteStModel idWriteWbSt [1] [Read 0] () @=? Right ()

case_model_read_write_stall_0 :: Assertion
case_model_read_write_stall_0 = do
  wishbonePropWithModelInner @System @Int @10 idWriteStModel idWriteWbSt [0, 0] [Read 0, Write 0 0] () @=? Right ()


tests :: TestTree
tests =
    -- TODO: Move timeout option to hedgehog for better error messages.
    -- TODO: Does not seem to work for combinatorial loops like @let x = x in x@??
    localOption (mkTimeout 12_000_000 {- 12 seconds -})
  $ localOption (HedgehogTestLimit (Just 1000))
  $(testGroupGenerator)
