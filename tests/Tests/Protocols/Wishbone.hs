{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ApplicativeDo       #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE NumericUnderscores  #-}
{-# LANGUAGE RecordWildCards     #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-} -- The exhaustiveness-checker doesn't work well with ViewPatterns
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
import           Prelude                        hiding (undefined)
import           Test.Tasty
import           Test.Tasty.Hedgehog            (HedgehogTestLimit (HedgehogTestLimit))
import           Test.Tasty.Hedgehog.Extra      (testProperty)
import           Test.Tasty.TH                  (testGroupGenerator)

smallInt :: Range Int
smallInt = Range.linear 0 10

genSmallInt :: Gen Int
genSmallInt = Gen.integral smallInt

genData :: Gen a -> Gen [a]
genData = Gen.list (Range.linear 0 150)

genWishboneTransfer :: (KnownNat addressWidth) => Gen a -> Gen (WishboneMasterRequest addressWidth a)
genWishboneTransfer genA = do
  Gen.choice [
      Read <$> genDefinedBitVector, Write <$> genDefinedBitVector <*> genA]

transfersToSignalsStandard
  :: forall addressWidth a . (KnownNat addressWidth, KnownNat (BitSize a), NFDataX a)
  => [WishboneMasterRequest addressWidth a]
  -> [WishboneM2S addressWidth (BitSize a `DivRU` 8) a]
transfersToSignalsStandard = Prelude.concatMap $ \case
   Read bv -> [ (wishboneM2S @addressWidth @a) { strobe = False, busCycle = False }
              , (wishboneM2S @addressWidth @a) { strobe = True, busCycle = True, addr = bv, writeEnable = False }]
   Write bv a -> [ wishboneM2S
                 , (wishboneM2S @addressWidth @a) { strobe = True, busCycle = True, addr = bv, writeEnable = True, writeData = a }]



--
-- 'id' circuit
--


idWriteWbSt :: forall a dom addressWidth. (BitPack a, NFDataX a) => Circuit (Wishbone dom 'Standard addressWidth a) ()
idWriteWbSt = Circuit go
  where
    go (m2s, ()) = (reply <$> m2s, ())

    reply WishboneM2S{..}
      | busCycle && strobe && writeEnable = (wishboneS2M @a) { acknowledge = True, readData = writeData }
      | busCycle && strobe                = wishboneS2M { acknowledge = True }
      | otherwise                         = wishboneS2M


idWriteStModel :: (NFData a, Eq a, ShowX a) => WishboneMasterRequest addressWidth a -> WishboneS2M a -> () -> Either String ()
idWriteStModel (Read _)    s@WishboneS2M{..} ()
  | acknowledge && isLeft (hasX readData) = Right ()
  | otherwise                             = Left $ "Read should have been acknowledged with no DAT " <> showX s
idWriteStModel (Write _ a) s@WishboneS2M{..} ()
  | acknowledge && hasX readData == Right a = Right ()
  | otherwise                               = Left $ "Write should have been acknowledged with write-data as DAT " <> showX s


prop_idWriteSt_model :: Property
prop_idWriteSt_model = wishbonePropWithModel @System defExpectOptions idWriteStModel idWriteWbSt (genData $ genWishboneTransfer @10 genSmallInt) ()


--
-- memory element circuit
--

memoryWb
  :: forall ramSize dom a addressWidth
  . (BitPack a, NFDataX a, 1 <= ramSize, KnownDomain dom, KnownNat addressWidth, HiddenClockResetEnable dom, KnownNat ramSize)
  => Circuit (Wishbone dom 'Standard addressWidth a) ()
memoryWb = Circuit go
  where
    go :: (Signal dom (WishboneM2S addressWidth (BitSize a `DivRU` 8) a), ()) -> (Signal dom (WishboneS2M a), ())
    go (m2s, ()) = (reply m2s, ())

    reply :: Signal dom (WishboneM2S addressWidth (BitSize a `DivRU` 8) a) -> Signal dom (WishboneS2M a)
    reply request = do
      ack' <- ack .&&. (strobe <$> request) .&&. (busCycle <$> request)
      val <- readValue
      pure $ (wishboneS2M @a) { acknowledge = ack', readData = val }
      where
        read' = addr <$> request
        writeData' = writeData <$> request
        write = mux ((writeEnable <$> request) .&&. (strobe <$> request) .&&. (busCycle <$> request)) (Just <$> ((,) <$> read' <*> writeData')) (pure Nothing)

        readValue = blockRamU ClearOnReset (SNat @ramSize) (const undefined) read' write
        ack = register False $ (strobe <$> request) .&&. (busCycle <$> request)

memoryWbModel
  :: (KnownNat addressWidth, Eq a, ShowX a, NFDataX a)
  => WishboneMasterRequest addressWidth a
  -> WishboneS2M a
  -> [(BitVector addressWidth, a)]
  -> Either String [(BitVector addressWidth, a)]
memoryWbModel (Read addr) s st@(lookup addr -> Just x )
  | readData s == x && acknowledge s = Right st
  | otherwise                        = Left $ "Read from a known address did not yield the same value " <> showX x <> " : " <> showX s
memoryWbModel (Read addr) s st@(lookup addr -> Nothing)
  | acknowledge s && isLeft (isX (readData s)) = Right st
  | otherwise                                  = Left $ "Read from unknown address did no ACK with undefined result : " <> showX s
memoryWbModel (Write addr a) s st
  | acknowledge s = Right ((addr, a):st)
  | otherwise     = Left $ "Write should be acked : " <> showX s


prop_memoryWb_model :: Property
prop_memoryWb_model =  withClockResetEnable clockGen resetGen enableGen $ wishbonePropWithModel @System
  defExpectOptions
  memoryWbModel
  (memoryWb @256)
  (genData (genWishboneTransfer @8 genSmallInt))
  []


tests :: TestTree
tests =
    -- TODO: Move timeout option to hedgehog for better error messages.
    -- TODO: Does not seem to work for combinatorial loops like @let x = x in x@??
    localOption (mkTimeout 12_000_000 {- 12 seconds -})
  $ localOption (HedgehogTestLimit (Just 1000))
  $(testGroupGenerator)
