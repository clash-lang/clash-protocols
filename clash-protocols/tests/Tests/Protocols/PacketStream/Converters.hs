{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE RecordWildCards #-}

module Tests.Protocols.PacketStream.Converters where

-- base
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
import Protocols.PacketStream.Converters

-- tests
import Tests.Protocols.PacketStream.Base

-- | Test the upconverter stream instance
upconverterTest :: forall n. (1 <= n) => C.SNat n -> Property
upconverterTest C.SNat =
  idWithModelSingleDomain
    @C.System
    defExpectOptions
    (genValidPackets (Range.linear 1 10) (Range.linear 1 20) Abort)
    (C.exposeClockResetEnable upConvert)
    (C.exposeClockResetEnable @C.System (upConverterC @n))

prop_upconverter_d1, prop_upconverter_d2, prop_upconverter_d4 :: Property
prop_upconverter_d1 = upconverterTest C.d1
prop_upconverter_d2 = upconverterTest C.d2
prop_upconverter_d4 = upconverterTest C.d4

generateDownConverterProperty ::
  forall (dwIn :: C.Nat) (dwOut :: C.Nat) (n :: C.Nat).
  (1 <= dwIn) =>
  (1 <= dwOut) =>
  (1 <= n) =>
  (C.KnownNat n) =>
  (dwIn ~ n C.* dwOut) =>
  C.SNat dwIn ->
  C.SNat dwOut ->
  Property
generateDownConverterProperty C.SNat C.SNat =
  idWithModelSingleDomain
    defExpectOptions{eoSampleMax = 1000}
    (genValidPackets (Range.linear 1 8) (Range.linear 1 10) Abort)
    (C.exposeClockResetEnable (upConvert . downConvert))
    (C.exposeClockResetEnable @C.System (downConverterC @dwIn @dwOut @Int))

prop_downConverter8to4 :: Property
prop_downConverter8to4 = generateDownConverterProperty C.d8 C.d4

prop_downConverter6to3 :: Property
prop_downConverter6to3 = generateDownConverterProperty C.d6 C.d3

prop_downConverter4to2 :: Property
prop_downConverter4to2 = generateDownConverterProperty C.d4 C.d2

prop_downConverter4to1 :: Property
prop_downConverter4to1 = generateDownConverterProperty C.d4 C.d1

prop_downConverter2to1 :: Property
prop_downConverter2to1 = generateDownConverterProperty C.d2 C.d1

prop_downConverter1to1 :: Property
prop_downConverter1to1 = generateDownConverterProperty C.d1 C.d1

tests :: TestTree
tests =
  localOption (mkTimeout 20_000_000 {- 20 seconds -}) $
    localOption
      (HedgehogTestLimit (Just 500))
      $(testGroupGenerator)
