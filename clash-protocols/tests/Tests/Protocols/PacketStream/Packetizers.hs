{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NumericUnderscores #-}

module Tests.Protocols.PacketStream.Packetizers (
  packetizerModel,
  packetizeFromDfModel,
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
-- import Hedgehog
-- import qualified Hedgehog.Gen as Gen
-- import qualified Hedgehog.Range as Range

-- tasty
import Test.Tasty
import Test.Tasty.Hedgehog (HedgehogTestLimit (HedgehogTestLimit))

-- import Test.Tasty.Hedgehog.Extra (testProperty)
import Test.Tasty.TH (testGroupGenerator)

-- clash-protocols
import Protocols.PacketStream.Base

-- tests
import Tests.Protocols.PacketStream.Base

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

tests :: TestTree
tests =
  localOption (mkTimeout 12_000_000 {- 12 seconds -}) $
    localOption
      (HedgehogTestLimit (Just 1_000_000))
      $(testGroupGenerator)
