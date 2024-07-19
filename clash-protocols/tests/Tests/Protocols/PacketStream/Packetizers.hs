{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NumericUnderscores #-}

module Tests.Protocols.PacketStream.Packetizers (
  packetizerModel,
  packetizeFromDfModel,
  tests,
) where

-- base
import qualified Data.List as L
import Prelude

-- clash
import Clash.Prelude
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
import qualified Protocols.Df as Df
import Protocols.Hedgehog
import Protocols.PacketStream.Base

-- tests

import Protocols.PacketStream (packetizeFromDfC, packetizerC)
import Tests.Protocols.PacketStream.Base

genVec :: (KnownNat n, 1 <= n) => Gen a -> Gen (Vec n a)
genVec gen = sequence (C.repeat gen)

genMeta ::
  forall (meta :: Type) (metaBytes :: Nat).
  (KnownNat metaBytes) =>
  (1 <= metaBytes) =>
  (BitPack meta) =>
  (BitSize meta ~ metaBytes * 8) =>
  Gen meta
genMeta = fmap bitCoerce (genVec Gen.enumBounded :: Gen (Vec metaBytes (BitVector 8)))

-- | Model of the generic `packetizerC`.
packetizerModel ::
  forall
    (dataWidth :: Nat)
    (headerBytes :: Nat)
    (metaIn :: Type)
    (header :: Type)
    (metaOut :: Type).
  (KnownNat dataWidth) =>
  (KnownNat headerBytes) =>
  (1 <= dataWidth) =>
  (1 <= headerBytes) =>
  (BitPack header) =>
  (BitSize header ~ headerBytes * 8) =>
  (metaIn -> metaOut) ->
  (metaIn -> header) ->
  [PacketStreamM2S dataWidth metaIn] ->
  [PacketStreamM2S dataWidth metaOut]
packetizerModel toMetaOut toHeader ps = L.concatMap (upConvert . prependHdr) bytePackets
 where
  prependHdr :: [PacketStreamM2S 1 metaIn] -> [PacketStreamM2S 1 metaOut]
  prependHdr fragments = hdr L.++ L.map (\f -> f{_meta = metaOut}) fragments
   where
    h = L.head fragments
    metaOut = toMetaOut (_meta h)
    hdr = L.map go (toList $ bitCoerce (toHeader (_meta h)))
    go byte = PacketStreamM2S (singleton byte) Nothing metaOut (_abort h)

  bytePackets :: [[PacketStreamM2S 1 metaIn]]
  bytePackets = downConvert . smearAbort <$> chunkByPacket ps

-- | Model of the generic `packetizeFromDfC`.
packetizeFromDfModel ::
  forall
    (dataWidth :: Nat)
    (headerBytes :: Nat)
    (a :: Type)
    (header :: Type)
    (metaOut :: Type).
  (KnownNat dataWidth) =>
  (KnownNat headerBytes) =>
  (1 <= dataWidth) =>
  (1 <= headerBytes) =>
  (BitPack header) =>
  (BitSize header ~ headerBytes * 8) =>
  (a -> metaOut) ->
  (a -> header) ->
  [a] ->
  [PacketStreamM2S dataWidth metaOut]
packetizeFromDfModel toMetaOut toHeader = L.concatMap (upConvert . packetize)
 where
  packetize :: a -> [PacketStreamM2S 1 metaOut]
  packetize d =
    fullPackets $
      L.map
        (\byte -> PacketStreamM2S (byte :> Nil) Nothing (toMetaOut d) False)
        (toList $ bitCoerce (toHeader d))

{- | Test the packetizer with varying datawidth and number of bytes in the header,
  with metaOut = ().
-}
packetizerPropertyGenerator ::
  forall
    (dataWidth :: Nat)
    (headerBytes :: Nat).
  (1 <= dataWidth) =>
  (1 <= headerBytes) =>
  SNat dataWidth ->
  SNat headerBytes ->
  Property
packetizerPropertyGenerator SNat SNat =
  idWithModelSingleDomain
    @System
    defExpectOptions
    (fmap (cleanPackets . fullPackets) (Gen.list (Range.linear 1 100) genPackets))
    (exposeClockResetEnable model)
    (exposeClockResetEnable ckt)
 where
  model ::
    [PacketStreamM2S dataWidth (Vec headerBytes (BitVector 8))] ->
    [PacketStreamM2S dataWidth ()]
  model = packetizerModel (const ()) id

  ckt ::
    (HiddenClockResetEnable System) =>
    Circuit
      (PacketStream System dataWidth (Vec headerBytes (BitVector 8)))
      (PacketStream System dataWidth ())
  ckt = packetizerC (const ()) id

  genPackets =
    PacketStreamM2S
      <$> genVec Gen.enumBounded
      <*> Gen.maybe Gen.enumBounded
      <*> genMeta
      <*> Gen.enumBounded

-- | headerBytes % dataWidth ~ 0
prop_const_packetizer_d1_d14 :: Property
prop_const_packetizer_d1_d14 = packetizerPropertyGenerator d1 d14

-- | dataWidth < headerBytes
prop_const_packetizer_d3_d11 :: Property
prop_const_packetizer_d3_d11 = packetizerPropertyGenerator d3 d11

-- | dataWidth ~ header byte size
prop_const_packetizer_d7_d7 :: Property
prop_const_packetizer_d7_d7 = packetizerPropertyGenerator d7 d7

-- | dataWidth > header byte size
prop_const_packetizer_d5_d4 :: Property
prop_const_packetizer_d5_d4 = packetizerPropertyGenerator d5 d4

{- | Test packetizeFromDf with varying datawidth and number of bytes in the header
  , with metaOut = ().
-}
packetizeFromDfPropertyGenerator ::
  forall
    (dataWidth :: Nat)
    (headerBytes :: Nat).
  (1 <= dataWidth) =>
  (1 <= headerBytes) =>
  SNat dataWidth ->
  SNat headerBytes ->
  Property
packetizeFromDfPropertyGenerator SNat SNat =
  idWithModelSingleDomain
    @System
    defExpectOptions
    (Gen.list (Range.linear 1 100) (genVec Gen.enumBounded))
    (exposeClockResetEnable model)
    (exposeClockResetEnable ckt)
 where
  model :: [Vec headerBytes (BitVector 8)] -> [PacketStreamM2S dataWidth ()]
  model = packetizeFromDfModel (const ()) id

  ckt ::
    (HiddenClockResetEnable System) =>
    Circuit (Df.Df System (Vec headerBytes (BitVector 8))) (PacketStream System dataWidth ())
  ckt = packetizeFromDfC (const ()) id

-- | headerBytes % dataWidth ~ 0
prop_const_packetizeFromDf_d1_d14 :: Property
prop_const_packetizeFromDf_d1_d14 = packetizeFromDfPropertyGenerator d1 d14

-- | dataWidth < headerBytes
prop_const_packetizeFromDf_d3_d11 :: Property
prop_const_packetizeFromDf_d3_d11 = packetizeFromDfPropertyGenerator d3 d11

-- | dataWidth ~ header byte size
prop_const_packetizeFromDf_d7_d7 :: Property
prop_const_packetizeFromDf_d7_d7 = packetizeFromDfPropertyGenerator d7 d7

-- | dataWidth > header byte size
prop_const_packetizeFromDf_d5_d4 :: Property
prop_const_packetizeFromDf_d5_d4 = packetizeFromDfPropertyGenerator d5 d4

tests :: TestTree
tests =
  localOption (mkTimeout 12_000_000 {- 12 seconds -}) $
    localOption
      (HedgehogTestLimit (Just 1_000))
      $(testGroupGenerator)
