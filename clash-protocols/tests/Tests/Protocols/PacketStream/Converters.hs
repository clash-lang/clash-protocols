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
import qualified Hedgehog.Range as Range

-- tasty
import Test.Tasty
import Test.Tasty.Hedgehog (HedgehogTestLimit (HedgehogTestLimit))
import Test.Tasty.Hedgehog.Extra (testProperty)
import Test.Tasty.TH (testGroupGenerator)

-- clash-protocols
import Protocols.Hedgehog
import Protocols.PacketStream.Base
import Protocols.PacketStream.Converters

-- tests
import Tests.Protocols.PacketStream.Base

ucModel :: forall n. (C.KnownNat n) => [PacketStreamM2S 1 ()] -> [PacketStreamM2S n ()]
ucModel fragments = out
 where
  wholePackets = smearAbort <$> chunkBy (M.isJust . _last) fragments
  chunks = wholePackets >>= chopBy (C.natToNum @n)
  out = fmap chunkToPacket chunks

-- | Test the upconverter stream instance
upconverterTest :: forall n. (1 <= n) => C.SNat n -> Property
upconverterTest C.SNat =
  idWithModelSingleDomain
    @C.System
    defExpectOptions
    (genValidPackets (Range.linear 1 30) (Range.linear 1 30) Abort)
    (C.exposeClockResetEnable ucModel)
    (C.exposeClockResetEnable @C.System (upConverterC @n))

prop_upconverter_d1, prop_upconverter_d2, prop_upconverter_d4 :: Property
prop_upconverter_d1 = upconverterTest C.d1
prop_upconverter_d2 = upconverterTest C.d2
prop_upconverter_d4 = upconverterTest C.d4

-- | Test the downconverter stream instance
downconverterTest :: forall n. (1 <= n) => C.SNat n -> Property
downconverterTest C.SNat =
  idWithModelSingleDomain
    @C.System
    defExpectOptions
    (genValidPackets (Range.linear 1 50) (Range.linear 1 10) Abort)
    (C.exposeClockResetEnable downConvert)
    (C.exposeClockResetEnable @C.System (downConverterC @n))

prop_downconverter_d1, prop_downconverter_d2, prop_downconverter_d4 :: Property
prop_downconverter_d1 = downconverterTest (C.SNat @1)
prop_downconverter_d2 = downconverterTest (C.SNat @2)
prop_downconverter_d4 = downconverterTest (C.SNat @4)

tests :: TestTree
tests =
  localOption (mkTimeout 20_000_000 {- 20 seconds -}) $
    localOption
      (HedgehogTestLimit (Just 1_000))
      $(testGroupGenerator)
