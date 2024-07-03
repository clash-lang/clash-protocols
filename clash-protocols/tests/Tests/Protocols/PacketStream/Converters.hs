{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE RecordWildCards #-}

module Tests.Protocols.PacketStream.Converters where

-- base
import qualified Data.Maybe as M
import Prelude

-- clash-prelude
import Clash.Prelude (type (<=))
import qualified Clash.Prelude as C

-- hedgehog
import Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

-- tasty
import Test.Tasty
import Test.Tasty.Hedgehog (HedgehogTestLimit (HedgehogTestLimit))
import Test.Tasty.Hedgehog.Extra (testProperty)
import Test.Tasty.TH (testGroupGenerator)

-- clash-protocols
import Protocols
import Protocols.Hedgehog
import Protocols.PacketStream.Base
import Protocols.PacketStream.Converters

-- tests
import Tests.Protocols.PacketStream.Base

genVec :: (C.KnownNat n, 1 <= n) => Gen a -> Gen (C.Vec n a)
genVec gen = sequence (C.repeat gen)

ucModel :: forall n. (C.KnownNat n) => [PacketStreamM2S 1 ()] -> [PacketStreamM2S n ()]
ucModel fragments = out
 where
  wholePackets = smearAbort <$> chunkBy (M.isJust . _last) fragments
  chunks = wholePackets >>= chopBy (C.natToNum @n)
  out = fmap chunkToPacket chunks

-- | Test the upconverter stream instance
upconverterTest :: forall n. (1 <= n) => C.SNat n -> Property
upconverterTest C.SNat =
  propWithModelSingleDomain
    @C.System
    defExpectOptions
    (fmap fullPackets (Gen.list (Range.linear 0 100) genPackets)) -- Input packets
    (C.exposeClockResetEnable ucModel) -- Desired behaviour of UpConverter
    (C.exposeClockResetEnable @C.System (ckt @n)) -- Implementation of UpConverter
    (===) -- Property to test
 where
  ckt ::
    forall (dataWidth :: C.Nat) (dom :: C.Domain).
    (C.HiddenClockResetEnable dom) =>
    (1 <= dataWidth) =>
    (C.KnownNat dataWidth) =>
    Circuit (PacketStream dom 1 ()) (PacketStream dom dataWidth ())
  ckt = upConverterC

  -- This generates the packets
  genPackets =
    PacketStreamM2S
      <$> genVec Gen.enumBounded
      <*> Gen.maybe Gen.enumBounded
      <*> Gen.enumBounded
      <*> Gen.enumBounded

prop_upconverter_d1, prop_upconverter_d2, prop_upconverter_d4 :: Property
prop_upconverter_d1 = upconverterTest (C.SNat @1)
prop_upconverter_d2 = upconverterTest (C.SNat @2)
prop_upconverter_d4 = upconverterTest (C.SNat @4)

dcModel ::
  forall n. (1 <= n) => (C.KnownNat n) => [PacketStreamM2S n ()] -> [PacketStreamM2S 1 ()]
dcModel = downConvert

-- | Test the downconverter stream instance
downconverterTest :: forall n. (1 <= n) => C.SNat n -> Property
downconverterTest C.SNat =
  propWithModelSingleDomain
    @C.System
    defExpectOptions
    (Gen.list (Range.linear 0 100) genPackets) -- Input packets
    (C.exposeClockResetEnable dcModel) -- Desired behaviour of DownConverter
    (C.exposeClockResetEnable @C.System (ckt @n)) -- Implementation of DownConverter
    (===) -- Property to test
 where
  ckt ::
    forall (dataWidth :: C.Nat) (dom :: C.Domain).
    (C.HiddenClockResetEnable dom) =>
    (1 <= dataWidth) =>
    (C.KnownNat dataWidth) =>
    Circuit (PacketStream dom dataWidth ()) (PacketStream dom 1 ())
  ckt = downConverterC

  -- This generates the packets
  genPackets =
    PacketStreamM2S
      <$> genVec Gen.enumBounded
      <*> Gen.maybe Gen.enumBounded
      <*> Gen.enumBounded
      <*> Gen.enumBounded

prop_downconverter_d1, prop_downconverter_d2, prop_downconverter_d4 :: Property
prop_downconverter_d1 = downconverterTest (C.SNat @1)
prop_downconverter_d2 = downconverterTest (C.SNat @2)
prop_downconverter_d4 = downconverterTest (C.SNat @4)

tests :: TestTree
tests =
  localOption (mkTimeout 12_000_000 {- 12 seconds -}) $
    localOption
      (HedgehogTestLimit (Just 1_000))
      $(testGroupGenerator)
