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
import           Protocols.Hedgehog             (ExpectOptions (eoTimeout),
                                                 defExpectOptions)
import           Protocols.Wishbone
import           Protocols.Wishbone.Hedgehog

-- tasty
import           Debug.Trace                    (trace)
import           Test.Tasty
import           Test.Tasty.HUnit
import           Test.Tasty.Hedgehog            (HedgehogTestLimit (HedgehogTestLimit))
import           Test.Tasty.Hedgehog.Extra      (testProperty)
import           Test.Tasty.TH                  (testGroupGenerator)
import           Text.Printf                    (printf)

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
  Gen.choice [Read <$> genBitVector, Write <$> genBitVector <*> genA]

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


idWriteWbPip :: forall a dom selWidth. (BitPack a, NFDataX a) => Circuit (Wishbone dom 'Pipelined selWidth a) ()
idWriteWbPip = Circuit go
  where
    go (m2s, ()) = (reply <$> m2s, ())

    reply m2s@WishboneM2S{..}
      | busCycle && strobe && writeEnable = (wishboneS2M @a) { acknowledge = True, readData = writeData }
      | busCycle && strobe                = wishboneS2M { acknowledge = True }
      | otherwise                         = wishboneS2M


prop_idWriteSt :: Property
prop_idWriteSt =  validateStallingStandardCircuit @System defExpectOptions genDat idWriteWbSt
  where
    genDat = transfersToSignalsStandard <$> genData (genWishboneTransfer @10 genSmallInt)


prop_idWritePip :: Property
prop_idWritePip = withClockResetEnable @System clockGen resetGen enableGen $ validateStallingPipelineCircuit @System (defExpectOptions { eoTimeout = Just 500 }) genDat idWriteWbPip
  where
    genDat = genData (genData (genWishboneTransfer @10 genSmallInt))

case_ack_and_stall_1_empty_cycle :: Assertion
case_ack_and_stall_1_empty_cycle =
  let val = withClockResetEnable @System clockGen resetGen enableGen $ validateStallingPipelineCircuitInner @System @Int @10 (defExpectOptions { eoTimeout = Just 500 }) [[]] [AckAndStall 1] idWriteWbPip
  in val @=? Right ()


tests :: TestTree
tests =
    -- TODO: Move timeout option to hedgehog for better error messages.
    -- TODO: Does not seem to work for combinatorial loops like @let x = x in x@??
    localOption (mkTimeout 30_000_000 {- 12 seconds -})
  $ localOption (HedgehogTestLimit (Just 1000))
  $(testGroupGenerator)
