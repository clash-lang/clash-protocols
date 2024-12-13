{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Tests.Protocols.PacketStream.Depacketizers (
  tests,
) where

import Clash.Prelude

import Hedgehog (Gen, Property)
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

import Test.Tasty
import Test.Tasty.Hedgehog (HedgehogTestLimit (HedgehogTestLimit))

import Test.Tasty.Hedgehog.Extra (testProperty)
import Test.Tasty.TH (testGroupGenerator)

import Protocols
import Protocols.Hedgehog
import Protocols.PacketStream.Base
import Protocols.PacketStream.Depacketizers
import Protocols.PacketStream.Hedgehog

{- |
Test @depacketizerC@ with varying data width, number of bytes in the
header, input metadata, and output metadata.
-}
depacketizerPropGen ::
  forall
    (metaIn :: Type)
    (metaOut :: Type)
    (dataWidth :: Nat)
    (headerBytes :: Nat).
  (1 <= dataWidth) =>
  (1 <= headerBytes) =>
  (TestType metaIn) =>
  (TestType metaOut) =>
  SNat dataWidth ->
  SNat headerBytes ->
  Gen metaIn ->
  (Vec headerBytes (BitVector 8) -> metaIn -> metaOut) ->
  Property
depacketizerPropGen SNat SNat metaGen toMetaOut =
  idWithModelSingleDomain
    @System
    defExpectOptions{eoSampleMax = 1000, eoStopAfterEmpty = 1000}
    (genPackets 1 4 (genValidPacket defPacketOptions metaGen (Range.linear 0 30)))
    (exposeClockResetEnable (depacketizerModel toMetaOut))
    (exposeClockResetEnable ckt)
 where
  ckt ::
    (HiddenClockResetEnable System) =>
    Circuit
      (PacketStream System dataWidth metaIn)
      (PacketStream System dataWidth metaOut)
  ckt = depacketizerC toMetaOut

{- |
Test @depacketizeToDfC@ with varying data width, number of bytes in the
header, input metadata, and output type @a@.
-}
depacketizeToDfPropGen ::
  forall
    (metaIn :: Type)
    (a :: Type)
    (dataWidth :: Nat)
    (headerBytes :: Nat).
  (1 <= dataWidth) =>
  (1 <= headerBytes) =>
  (BitPack a) =>
  (BitSize a ~ headerBytes * 8) =>
  (TestType a) =>
  (TestType metaIn) =>
  SNat dataWidth ->
  SNat headerBytes ->
  Gen metaIn ->
  (Vec headerBytes (BitVector 8) -> metaIn -> a) ->
  Property
depacketizeToDfPropGen SNat SNat metaGen toOut =
  idWithModelSingleDomain
    @System
    defExpectOptions{eoSampleMax = 1000, eoStopAfterEmpty = 1000}
    (genPackets 1 10 (genValidPacket defPacketOptions metaGen (Range.linear 0 20)))
    (exposeClockResetEnable (depacketizeToDfModel toOut))
    (exposeClockResetEnable ckt)
 where
  ckt ::
    (HiddenClockResetEnable System) =>
    Circuit (PacketStream System dataWidth metaIn) (Df System a)
  ckt = depacketizeToDfC toOut

{- |
Do something interesting with both the parsed header and the input
metadata for testing purposes. We just xor every byte in the parsed
header with the byte in the input metadata.
-}
exampleToMetaOut ::
  Vec headerBytes (BitVector 8) ->
  BitVector 8 ->
  Vec headerBytes (BitVector 8)
exampleToMetaOut hdr metaIn = map (`xor` metaIn) hdr

-- | headerBytes % dataWidth ~ 0
prop_const_depacketizer_d1_d14 :: Property
prop_const_depacketizer_d1_d14 =
  depacketizerPropGen d1 d14 (pure ()) const

prop_xor_depacketizer_d1_d14 :: Property
prop_xor_depacketizer_d1_d14 =
  depacketizerPropGen d1 d14 Gen.enumBounded exampleToMetaOut

-- | dataWidth < headerBytes
prop_const_depacketizer_d3_d11 :: Property
prop_const_depacketizer_d3_d11 =
  depacketizerPropGen d3 d11 (pure ()) const

prop_xor_depacketizer_d3_d11 :: Property
prop_xor_depacketizer_d3_d11 =
  depacketizerPropGen d3 d11 Gen.enumBounded exampleToMetaOut

-- | dataWidth ~ header byte size
prop_const_depacketizer_d7_d7 :: Property
prop_const_depacketizer_d7_d7 =
  depacketizerPropGen d7 d7 (pure ()) const

prop_xor_depacketizer_d7_d7 :: Property
prop_xor_depacketizer_d7_d7 =
  depacketizerPropGen d7 d7 Gen.enumBounded exampleToMetaOut

-- | dataWidth > header byte size
prop_const_depacketizer_d5_d4 :: Property
prop_const_depacketizer_d5_d4 =
  depacketizerPropGen d5 d4 (pure ()) const

prop_xor_depacketizer_d5_d4 :: Property
prop_xor_depacketizer_d5_d4 =
  depacketizerPropGen d5 d4 Gen.enumBounded exampleToMetaOut

-- | headerBytes % dataWidth ~ 0
prop_const_depacketize_to_df_d1_d14 :: Property
prop_const_depacketize_to_df_d1_d14 =
  depacketizeToDfPropGen d1 d14 (pure ()) const

prop_xor_depacketize_to_df_d1_d14 :: Property
prop_xor_depacketize_to_df_d1_d14 =
  depacketizeToDfPropGen d1 d14 Gen.enumBounded exampleToMetaOut

-- | dataWidth < headerBytes
prop_const_depacketize_to_df_d3_d11 :: Property
prop_const_depacketize_to_df_d3_d11 =
  depacketizeToDfPropGen d3 d11 (pure ()) const

prop_xor_depacketize_to_df_d3_d11 :: Property
prop_xor_depacketize_to_df_d3_d11 =
  depacketizeToDfPropGen d3 d11 Gen.enumBounded exampleToMetaOut

-- | dataWidth ~ header byte size
prop_const_depacketize_to_df_d7_d7 :: Property
prop_const_depacketize_to_df_d7_d7 =
  depacketizeToDfPropGen d7 d7 (pure ()) const

prop_xor_depacketize_to_df_d7_d7 :: Property
prop_xor_depacketize_to_df_d7_d7 =
  depacketizeToDfPropGen d7 d7 Gen.enumBounded exampleToMetaOut

-- | dataWidth > header byte size
prop_const_depacketize_to_df_d5_d4 :: Property
prop_const_depacketize_to_df_d5_d4 =
  depacketizeToDfPropGen d5 d4 (pure ()) const

prop_xor_depacketize_to_df_d5_d4 :: Property
prop_xor_depacketize_to_df_d5_d4 =
  depacketizeToDfPropGen d5 d4 Gen.enumBounded exampleToMetaOut

tests :: TestTree
tests =
  localOption (mkTimeout 20_000_000 {- 20 seconds -})
    $ localOption
      (HedgehogTestLimit (Just 500))
      $(testGroupGenerator)
