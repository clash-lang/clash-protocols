{-# LANGUAGE NumericUnderscores #-}

module Tests.Protocols.PacketStream.Converters (
  tests,
) where

import Clash.Prelude

import Hedgehog (Property)
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

import Test.Tasty
import Test.Tasty.Hedgehog (HedgehogTestLimit (HedgehogTestLimit))
import Test.Tasty.Hedgehog.Extra (testProperty)
import Test.Tasty.TH (testGroupGenerator)

import Protocols.Hedgehog
import Protocols.PacketStream.Converters
import Protocols.PacketStream.Hedgehog

generateUpConverterProperty ::
  forall (dwIn :: Nat) (n :: Nat).
  (1 <= dwIn) =>
  (1 <= n) =>
  (1 <= dwIn * n) =>
  SNat dwIn ->
  SNat n ->
  Property
generateUpConverterProperty SNat SNat =
  idWithModelSingleDomain
    defExpectOptions
    (genPackets 1 10 (genValidPacket defPacketOptions Gen.enumBounded (Range.linear 0 20)))
    (exposeClockResetEnable (upConvert . downConvert))
    (exposeClockResetEnable @System (upConverterC @dwIn @n @Int))

generateDownConverterProperty ::
  forall (dwOut :: Nat) (n :: Nat).
  (1 <= dwOut) =>
  (1 <= n) =>
  (1 <= dwOut * n) =>
  SNat dwOut ->
  SNat n ->
  Property
generateDownConverterProperty SNat SNat =
  idWithModelSingleDomain
    defExpectOptions{eoSampleMax = 1000}
    (genPackets 1 8 (genValidPacket defPacketOptions Gen.enumBounded (Range.linear 0 10)))
    (exposeClockResetEnable (upConvert . downConvert))
    (exposeClockResetEnable @System (downConverterC @dwOut @n @Int))

prop_upConverter3to9 :: Property
prop_upConverter3to9 = generateUpConverterProperty d3 d3

prop_upConverter4to8 :: Property
prop_upConverter4to8 = generateUpConverterProperty d4 d2

prop_upConverter3to6 :: Property
prop_upConverter3to6 = generateUpConverterProperty d3 d2

prop_upConverter2to4 :: Property
prop_upConverter2to4 = generateUpConverterProperty d2 d2

prop_upConverter1to4 :: Property
prop_upConverter1to4 = generateUpConverterProperty d1 d4

prop_upConverter1to2 :: Property
prop_upConverter1to2 = generateUpConverterProperty d1 d2

prop_upConverter1to1 :: Property
prop_upConverter1to1 = generateUpConverterProperty d1 d1

prop_downConverter9to3 :: Property
prop_downConverter9to3 = generateDownConverterProperty d3 d3

prop_downConverter8to4 :: Property
prop_downConverter8to4 = generateDownConverterProperty d4 d2

prop_downConverter6to3 :: Property
prop_downConverter6to3 = generateDownConverterProperty d3 d2

prop_downConverter4to2 :: Property
prop_downConverter4to2 = generateDownConverterProperty d2 d2

prop_downConverter4to1 :: Property
prop_downConverter4to1 = generateDownConverterProperty d1 d4

prop_downConverter2to1 :: Property
prop_downConverter2to1 = generateDownConverterProperty d1 d2

prop_downConverter1to1 :: Property
prop_downConverter1to1 = generateDownConverterProperty d1 d1

tests :: TestTree
tests =
  localOption (mkTimeout 20_000_000 {- 20 seconds -}) $
    localOption
      (HedgehogTestLimit (Just 500))
      $(testGroupGenerator)
