{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Tests.Protocols.PacketStream.Packetizers (
  tests,
) where

import Clash.Hedgehog.Sized.Vector (genVec)
import Clash.Prelude

import Hedgehog (Property)
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

import Test.Tasty
import Test.Tasty.Hedgehog (HedgehogTestLimit (HedgehogTestLimit))
import Test.Tasty.Hedgehog.Extra (testProperty)
import Test.Tasty.TH (testGroupGenerator)

import Protocols
import qualified Protocols.Df as Df
import Protocols.Hedgehog
import Protocols.PacketStream (packetizeFromDfC, packetizerC)
import Protocols.PacketStream.Base
import Protocols.PacketStream.Hedgehog

{- |
Test @packetizerC@ with varying data width, number of bytes in the
header, input metadata, and output metadata.

We consider the input metadata to be @Vec metaInBytes (BitVector 8)@ to
avoid unnecessary conversions, because @packetizerC@ requires that the
input metadata is convertible to this type anyway.
-}
packetizerPropGen ::
  forall
    (dataWidth :: Nat)
    (headerBytes :: Nat)
    (metaInBytes :: Nat)
    (metaOut :: Type).
  (KnownNat metaInBytes) =>
  (1 <= dataWidth) =>
  (1 <= headerBytes) =>
  (TestType metaOut) =>
  SNat dataWidth ->
  SNat headerBytes ->
  (Vec metaInBytes (BitVector 8) -> metaOut) ->
  (Vec metaInBytes (BitVector 8) -> Vec headerBytes (BitVector 8)) ->
  Property
packetizerPropGen SNat SNat toMetaOut toHeader =
  idWithModelSingleDomain
    @System
    defExpectOptions
    ( genPackets
        1
        10
        (genValidPacket defPacketOptions (genVec Gen.enumBounded) (Range.linear 0 10))
    )
    (exposeClockResetEnable model)
    (exposeClockResetEnable ckt)
 where
  model = packetizerModel toMetaOut toHeader
  ckt ::
    (HiddenClockResetEnable System) =>
    Circuit
      (PacketStream System dataWidth (Vec metaInBytes (BitVector 8)))
      (PacketStream System dataWidth metaOut)
  ckt = packetizerC toMetaOut toHeader

{- |
Test @packetizeFromDfC@ with varying data width, number of bytes in the
header, input type, and output metadata.

We consider the input type to be @Vec aBytes (BitVector 8)@ to
avoid unnecessary conversions, because @packetizerC@ requires that the
input type is convertible to this type anyway.
-}
packetizeFromDfPropGen ::
  forall
    (dataWidth :: Nat)
    (headerBytes :: Nat)
    (aBytes :: Nat)
    (metaOut :: Type).
  (KnownNat aBytes) =>
  (1 <= dataWidth) =>
  (1 <= headerBytes) =>
  (TestType metaOut) =>
  SNat dataWidth ->
  SNat headerBytes ->
  (Vec aBytes (BitVector 8) -> metaOut) ->
  (Vec aBytes (BitVector 8) -> Vec headerBytes (BitVector 8)) ->
  Property
packetizeFromDfPropGen SNat SNat toMetaOut toHeader =
  idWithModelSingleDomain
    @System
    defExpectOptions
    (Gen.list (Range.linear 1 10) (genVec Gen.enumBounded))
    (exposeClockResetEnable model)
    (exposeClockResetEnable ckt)
 where
  model = packetizeFromDfModel toMetaOut toHeader
  ckt ::
    (HiddenClockResetEnable System) =>
    Circuit
      (Df.Df System (Vec aBytes (BitVector 8)))
      (PacketStream System dataWidth metaOut)
  ckt = packetizeFromDfC toMetaOut toHeader

{- |
Do something interesting with the input metadata to derive the output
metadata for testing purposes. We just xor-reduce the input metadata.
-}
myToMetaOut :: Vec n (BitVector 8) -> BitVector 8
myToMetaOut = foldr xor 0

{- |
Do something interesting with the input metadata to derive the header
for testing purposes. We just xor every byte in the input metadata with
an arbitrary constant and add some bytes.
-}
myToHeader ::
  forall metaInBytes headerBytes.
  (2 + metaInBytes ~ headerBytes) =>
  Vec metaInBytes (BitVector 8) ->
  Vec headerBytes (BitVector 8)
myToHeader metaIn = map (`xor` 0xAB) metaIn ++ (0x01 :> 0x02 :> Nil)

-- | headerBytes % dataWidth ~ 0
prop_const_packetizer_d1_d14 :: Property
prop_const_packetizer_d1_d14 =
  packetizerPropGen d1 d14 (const ()) id

prop_xor_packetizer_d1_d14 :: Property
prop_xor_packetizer_d1_d14 =
  packetizerPropGen d1 d14 myToMetaOut myToHeader

-- | dataWidth < headerBytes
prop_const_packetizer_d3_d11 :: Property
prop_const_packetizer_d3_d11 =
  packetizerPropGen d3 d11 (const ()) id

prop_xor_packetizer_d3_d11 :: Property
prop_xor_packetizer_d3_d11 =
  packetizerPropGen d3 d11 myToMetaOut myToHeader

-- | dataWidth ~ header byte size
prop_const_packetizer_d7_d7 :: Property
prop_const_packetizer_d7_d7 =
  packetizerPropGen d7 d7 (const ()) id

prop_xor_packetizer_d7_d7 :: Property
prop_xor_packetizer_d7_d7 =
  packetizerPropGen d7 d7 myToMetaOut myToHeader

-- | dataWidth > header byte size
prop_const_packetizer_d5_d4 :: Property
prop_const_packetizer_d5_d4 =
  packetizerPropGen d5 d4 (const ()) id

prop_xor_packetizer_d5_d4 :: Property
prop_xor_packetizer_d5_d4 =
  packetizerPropGen d5 d4 myToMetaOut myToHeader

-- | headerBytes % dataWidth ~ 0
prop_const_packetizeFromDf_d1_d14 :: Property
prop_const_packetizeFromDf_d1_d14 =
  packetizeFromDfPropGen d1 d14 (const ()) id

prop_xor_packetizeFromDf_d1_d14 :: Property
prop_xor_packetizeFromDf_d1_d14 =
  packetizeFromDfPropGen d1 d14 myToMetaOut myToHeader

-- | dataWidth < headerBytes
prop_const_packetizeFromDf_d3_d11 :: Property
prop_const_packetizeFromDf_d3_d11 =
  packetizeFromDfPropGen d3 d11 (const ()) id

prop_xor_packetizeFromDf_d3_d11 :: Property
prop_xor_packetizeFromDf_d3_d11 =
  packetizeFromDfPropGen d3 d11 myToMetaOut myToHeader

-- | dataWidth ~ header byte size
prop_const_packetizeFromDf_d7_d7 :: Property
prop_const_packetizeFromDf_d7_d7 =
  packetizeFromDfPropGen d7 d7 (const ()) id

prop_xor_packetizeFromDf_d7_d7 :: Property
prop_xor_packetizeFromDf_d7_d7 =
  packetizeFromDfPropGen d7 d7 myToMetaOut myToHeader

-- | dataWidth > header byte size
prop_const_packetizeFromDf_d5_d4 :: Property
prop_const_packetizeFromDf_d5_d4 =
  packetizeFromDfPropGen d5 d4 (const ()) id

prop_xor_packetizeFromDf_d5_d4 :: Property
prop_xor_packetizeFromDf_d5_d4 =
  packetizeFromDfPropGen d5 d4 myToMetaOut myToHeader

tests :: TestTree
tests =
  localOption (mkTimeout 20_000_000 {- 20 seconds -})
    $ localOption
      (HedgehogTestLimit (Just 1_000))
      $(testGroupGenerator)
