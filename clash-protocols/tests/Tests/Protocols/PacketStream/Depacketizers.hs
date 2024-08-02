{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NumericUnderscores #-}

module Tests.Protocols.PacketStream.Depacketizers (
  depacketizerModel,
  depacketizeToDfModel,
  tests,
) where

-- base
import qualified Data.List as L
import Prelude

-- clash
import Clash.Prelude
import Clash.Sized.Vector (unsafeFromList)

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

-- tests
import Protocols
import Protocols.PacketStream (depacketizeToDfC, depacketizerC)
import Tests.Protocols.PacketStream.Base

-- | Model of the generic `depacketizerC`.
depacketizerModel ::
  forall
    (dataWidth :: Nat)
    (headerBytes :: Nat)
    (metaIn :: Type)
    (metaOut :: Type)
    (header :: Type).
  ( KnownNat dataWidth
  , KnownNat headerBytes
  , 1 <= dataWidth
  , 1 <= headerBytes
  , BitPack header
  , BitSize header ~ headerBytes * 8
  ) =>
  (header -> metaIn -> metaOut) ->
  [PacketStreamM2S dataWidth metaIn] ->
  [PacketStreamM2S dataWidth metaOut]
depacketizerModel toMetaOut ps = L.concat dataWidthPackets
 where
  hdrbytes = natToNum @headerBytes

  parseHdr ::
    ([PacketStreamM2S 1 metaIn], [PacketStreamM2S 1 metaIn]) -> [PacketStreamM2S 1 metaOut]
  parseHdr (hdrF, fwdF) = fmap (\f -> f{_meta = metaOut}) fwdF
   where
    hdr = bitCoerce $ unsafeFromList @headerBytes $ _data <$> hdrF
    metaOut = toMetaOut hdr (_meta $ L.head fwdF)

  bytePackets :: [[PacketStreamM2S 1 metaIn]]
  bytePackets =
    L.filter (\fs -> L.length fs > hdrbytes) $
      L.concatMap chopPacket . smearAbort <$> chunkByPacket ps

  parsedPackets :: [[PacketStreamM2S 1 metaOut]]
  parsedPackets = parseHdr . L.splitAt hdrbytes <$> bytePackets

  dataWidthPackets :: [[PacketStreamM2S dataWidth metaOut]]
  dataWidthPackets = fmap chunkToPacket . chopBy (natToNum @dataWidth) <$> parsedPackets

-- | Model of the generic `depacketizeToDfC`.
depacketizeToDfModel ::
  forall
    (dataWidth :: Nat)
    (headerBytes :: Nat)
    (meta :: Type)
    (a :: Type)
    (header :: Type).
  ( KnownNat dataWidth
  , KnownNat headerBytes
  , 1 <= dataWidth
  , 1 <= headerBytes
  , BitPack header
  , BitSize header ~ headerBytes * 8
  ) =>
  (header -> meta -> a) ->
  [PacketStreamM2S dataWidth meta] ->
  [a]
depacketizeToDfModel toOut ps = parseHdr <$> bytePackets
 where
  hdrbytes = natToNum @headerBytes

  parseHdr :: [PacketStreamM2S 1 meta] -> a
  parseHdr hdrF = toOut (bitCoerce $ unsafeFromList @headerBytes $ _data <$> hdrF) (_meta $ L.head hdrF)

  bytePackets :: [[PacketStreamM2S 1 meta]]
  bytePackets =
    L.filter (\fs -> L.length fs >= hdrbytes) $
      L.concatMap chopPacket <$> chunkByPacket (dropAbortedPackets ps)

{- | Test the depacketizer with varying datawidth and number of bytes in the header,
  with metaIn = () and toMetaOut = const.
-}
depacketizerPropertyGenerator ::
  forall
    (dataWidth :: Nat)
    (headerBytes :: Nat).
  (1 <= dataWidth) =>
  (1 <= headerBytes) =>
  SNat dataWidth ->
  SNat headerBytes ->
  Property
depacketizerPropertyGenerator SNat SNat =
  idWithModelSingleDomain
    @System
    defExpectOptions{eoSampleMax = 1000, eoStopAfterEmpty = 1000}
    (genValidPackets (Range.linear 1 4) (Range.linear 1 30) Abort)
    (exposeClockResetEnable model)
    (exposeClockResetEnable ckt)
 where
  model = depacketizerModel const
  ckt ::
    (HiddenClockResetEnable System) =>
    Circuit
      (PacketStream System dataWidth ())
      (PacketStream System dataWidth (Vec headerBytes (BitVector 8)))
  ckt = depacketizerC const

-- | headerBytes % dataWidth ~ 0
prop_const_depacketizer_d1_d14 :: Property
prop_const_depacketizer_d1_d14 = depacketizerPropertyGenerator d1 d14

-- | dataWidth < headerBytes
prop_const_depacketizer_d3_d11 :: Property
prop_const_depacketizer_d3_d11 = depacketizerPropertyGenerator d3 d11

-- | dataWidth ~ header byte size
prop_const_depacketizer_d7_d7 :: Property
prop_const_depacketizer_d7_d7 = depacketizerPropertyGenerator d7 d7

-- | dataWidth > header byte size
prop_const_depacketizer_d5_d4 :: Property
prop_const_depacketizer_d5_d4 = depacketizerPropertyGenerator d5 d4

{- | Test depacketizeToDf with varying datawidth and number of bytes in the header,
  with metaIn = () and toMetaOut = const.
-}
depacketizeToDfPropertyGenerator ::
  forall
    (dataWidth :: Nat)
    (headerBytes :: Nat).
  (1 <= dataWidth) =>
  (1 <= headerBytes) =>
  SNat dataWidth ->
  SNat headerBytes ->
  Property
depacketizeToDfPropertyGenerator SNat SNat =
  idWithModelSingleDomain
    @System
    defExpectOptions{eoSampleMax = 1000, eoStopAfterEmpty = 1000}
    (genValidPackets (Range.linear 1 4) (Range.linear 1 30) NoAbort)
    (exposeClockResetEnable model)
    (exposeClockResetEnable ckt)
 where
  model = depacketizeToDfModel const
  ckt ::
    (HiddenClockResetEnable System) =>
    Circuit (PacketStream System dataWidth ()) (Df System (Vec headerBytes (BitVector 8)))
  ckt = depacketizeToDfC const

-- | headerBytes % dataWidth ~ 0
prop_const_depacketize_to_df_d1_d14 :: Property
prop_const_depacketize_to_df_d1_d14 = depacketizeToDfPropertyGenerator d1 d14

-- | dataWidth < headerBytes
prop_const_depacketize_to_df_d3_d11 :: Property
prop_const_depacketize_to_df_d3_d11 = depacketizeToDfPropertyGenerator d3 d11

-- | dataWidth ~ header byte size
prop_const_depacketize_to_df_d7_d7 :: Property
prop_const_depacketize_to_df_d7_d7 = depacketizeToDfPropertyGenerator d7 d7

-- | dataWidth > header byte size
prop_const_depacketize_to_df_d5_d4 :: Property
prop_const_depacketize_to_df_d5_d4 = depacketizeToDfPropertyGenerator d5 d4

tests :: TestTree
tests =
  localOption (mkTimeout 20_000_000 {- 20 seconds -}) $
    localOption
      (HedgehogTestLimit (Just 100))
      $(testGroupGenerator)
