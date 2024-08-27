{-# LANGUAGE RecordWildCards #-}

{- |
Copyright   : (C) 2024, QBayLogic B.V.
License     : BSD2 (see the file LICENSE)
Maintainer  : QBayLogic B.V.

Provides Hedgehog generators, models and utility functions for testing
`PacketStream` circuits.
-}
module Protocols.PacketStream.Hedgehog (
  -- * Utility functions
  chopPacket,
  chunkByPacket,
  chunkToPacket,
  fullPackets,
  smearAbort,

  -- * Models
  dropAbortedPackets,
  downConvert,
  upConvert,
  depacketizerModel,
  depacketizeToDfModel,
  packetize,
  packetizeFromDf,

  -- * Hedgehog generators
  AbortMode (..),
  genValidPacket,
  genValidPackets,
) where

import Prelude

import Clash.Hedgehog.Sized.Vector (genVec)
import qualified Clash.Prelude as C
import qualified Clash.Sized.Vector as Vec

import Hedgehog (Gen, Range)
import qualified Hedgehog.Gen as Gen

import Protocols.PacketStream.Base

import qualified Data.List as L
import Data.Maybe (fromJust, isJust)

-- | Partition a list based on given function.
chunkBy :: (a -> Bool) -> [a] -> [[a]]
chunkBy _ [] = []
chunkBy predicate list = L.filter (not . null) (chunkByHelper predicate list [])

-- Helper function to accumulate chunks.
chunkByHelper :: (a -> Bool) -> [a] -> [a] -> [[a]]
chunkByHelper _ [] acc = [L.reverse acc]
chunkByHelper predicate (x : xs) acc
  | predicate x = L.reverse (x : acc) : chunkByHelper predicate xs []
  | otherwise = chunkByHelper predicate xs (x : acc)

-- | Partition a list of `PacketStream` transfers into complete packets.
chunkByPacket ::
  [PacketStreamM2S dataWidth meta] ->
  [[PacketStreamM2S dataWidth meta]]
chunkByPacket = chunkBy (isJust . _last)

{- |
If a packet contains a transfer with `_abort` set, set the `_abort` of
all following transfers in the same packet.
-}
smearAbort ::
  [PacketStreamM2S dataWidth meta] ->
  [PacketStreamM2S dataWidth meta]
smearAbort [] = []
smearAbort (x : xs) = L.reverse $ L.foldl' go [x] xs
 where
  go [] _ = []
  go l@(a : _) (PacketStreamM2S dat last' meta abort) =
    PacketStreamM2S dat last' meta (_abort a || abort) : l

-- | Partition a list into groups of given size
chopBy :: Int -> [a] -> [[a]]
chopBy _ [] = []
chopBy n xs = as : chopBy n bs where (as, bs) = splitAt n xs

{- |
Merge a list of `PacketStream` transfers with data width @1@ to
a single `PacketStream` transfer with data width @dataWidth@.
-}
chunkToPacket ::
  (C.KnownNat dataWidth) =>
  [PacketStreamM2S 1 meta] ->
  PacketStreamM2S dataWidth meta
chunkToPacket l =
  PacketStreamM2S
    { _last =
        if isJust (_last lastTransfer)
          then Just (fromIntegral $ L.length l - 1)
          else Nothing
    , _abort = any _abort l
    , _meta = _meta lastTransfer
    , _data = foldr ((C.+>>) . C.head . _data) (C.repeat 0) l
    }
 where
  lastTransfer = L.last l

{- |
Split a single `PacketStream` transfer with data width @dataWidth@ to
a list of `PacketStream` transfers with data width @1@.
-}
chopPacket ::
  forall dataWidth meta.
  (1 C.<= dataWidth) =>
  (C.KnownNat dataWidth) =>
  PacketStreamM2S dataWidth meta ->
  [PacketStreamM2S 1 meta]
chopPacket PacketStreamM2S{..} = packets
 where
  lasts = case _last of
    Nothing -> repeat Nothing
    Just in' -> replicate (fromIntegral in') Nothing ++ [Just (0 :: C.Index 1)]

  datas = case _last of
    Nothing -> C.toList _data
    Just in' -> take (fromIntegral in' + 1) $ C.toList _data

  packets =
    ( \(idx, dat) ->
        PacketStreamM2S (pure dat) idx _meta _abort
    )
      <$> zip lasts datas

-- | Set `_last` of the last transfer in the list to @Just 0@
fullPackets ::
  (C.KnownNat dataWidth) =>
  [PacketStreamM2S dataWidth meta] ->
  [PacketStreamM2S dataWidth meta]
fullPackets [] = []
fullPackets fragments =
  let lastFragment = (last fragments){_last = Just 0}
   in init fragments ++ [lastFragment]

-- | Drops packets if one of the transfers in the packet has `_abort` set.
dropAbortedPackets ::
  [PacketStreamM2S dataWidth meta] ->
  [PacketStreamM2S dataWidth meta]
dropAbortedPackets packets = concat $ filter (not . any _abort) (chunkByPacket packets)

{- |
Splits a list of `PacketStream` transfers with data width @1@ into
a list of `PacketStream` transfers with data width @dataWidth@
-}
downConvert ::
  forall dataWidth meta.
  (1 C.<= dataWidth) =>
  (C.KnownNat dataWidth) =>
  [PacketStreamM2S dataWidth meta] ->
  [PacketStreamM2S 1 meta]
downConvert = concatMap chopPacket

{- |
Merges a list of `PacketStream` transfers with data width @dataWidth into
a list of `PacketStream` transfers with data width @1@
-}
upConvert ::
  forall dataWidth meta.
  (1 C.<= dataWidth) =>
  (C.KnownNat dataWidth) =>
  [PacketStreamM2S 1 meta] ->
  [PacketStreamM2S dataWidth meta]
upConvert packets =
  map
    chunkToPacket
    (chunkByPacket packets >>= chopBy (C.natToNum @dataWidth))

-- | Model of the generic `Protocols.PacketStream.depacketizerC`.
depacketizerModel ::
  forall
    (dataWidth :: C.Nat)
    (headerBytes :: C.Nat)
    (metaIn :: C.Type)
    (header :: C.Type)
    (metaOut :: C.Type).
  (C.KnownNat dataWidth) =>
  (C.KnownNat headerBytes) =>
  (1 C.<= dataWidth) =>
  (1 C.<= headerBytes) =>
  (C.BitPack header) =>
  (C.BitSize header ~ headerBytes C.* 8) =>
  (header -> metaIn -> metaOut) ->
  [PacketStreamM2S dataWidth metaIn] ->
  [PacketStreamM2S dataWidth metaOut]
depacketizerModel toMetaOut ps = L.concat dataWidthPackets
 where
  hdrbytes = C.natToNum @headerBytes

  parseHdr ::
    ([PacketStreamM2S 1 metaIn], [PacketStreamM2S 1 metaIn]) ->
    [PacketStreamM2S 1 metaOut]
  parseHdr (hdrF, fwdF) = fmap (\f -> f{_meta = metaOut}) fwdF
   where
    hdr = C.bitCoerce $ Vec.unsafeFromList @headerBytes $ _data <$> hdrF
    metaOut = toMetaOut hdr (_meta $ L.head fwdF)

  bytePackets :: [[PacketStreamM2S 1 metaIn]]
  bytePackets =
    L.filter (\fs -> L.length fs > hdrbytes) $
      L.concatMap chopPacket . smearAbort <$> chunkByPacket ps

  parsedPackets :: [[PacketStreamM2S 1 metaOut]]
  parsedPackets = parseHdr . L.splitAt hdrbytes <$> bytePackets

  dataWidthPackets :: [[PacketStreamM2S dataWidth metaOut]]
  dataWidthPackets = L.map upConvert parsedPackets

-- | Model of the generic `Protocols.PacketStream.depacketizeToDfC`.
depacketizeToDfModel ::
  forall
    (dataWidth :: C.Nat)
    (headerBytes :: C.Nat)
    (a :: C.Type)
    (header :: C.Type)
    (metaIn :: C.Type).
  (C.KnownNat dataWidth) =>
  (C.KnownNat headerBytes) =>
  (1 C.<= dataWidth) =>
  (1 C.<= headerBytes) =>
  (C.BitPack header) =>
  (C.BitSize header ~ headerBytes C.* 8) =>
  (header -> metaIn -> a) ->
  [PacketStreamM2S dataWidth metaIn] ->
  [a]
depacketizeToDfModel toOut ps = L.map parseHdr bytePackets
 where
  parseHdr :: [PacketStreamM2S 1 metaIn] -> a
  parseHdr hdrF =
    toOut
      (C.bitCoerce $ Vec.unsafeFromList $ _data <$> hdrF)
      (_meta $ L.head hdrF)

  bytePackets :: [[PacketStreamM2S 1 metaIn]]
  bytePackets =
    L.filter
      (\pkt -> L.length pkt >= C.natToNum @headerBytes)
      (chunkByPacket $ downConvert (dropAbortedPackets ps))

-- | Model of the generic `Protocols.PacketStream.packetizerC`.
packetize ::
  forall
    (dataWidth :: C.Nat)
    (headerBytes :: C.Nat)
    (metaIn :: C.Type)
    (header :: C.Type)
    (metaOut :: C.Type).
  (C.KnownNat dataWidth) =>
  (C.KnownNat headerBytes) =>
  (1 C.<= dataWidth) =>
  (1 C.<= headerBytes) =>
  (C.BitPack header) =>
  (C.BitSize header ~ headerBytes C.* 8) =>
  (metaIn -> metaOut) ->
  (metaIn -> header) ->
  [PacketStreamM2S dataWidth metaIn] ->
  [PacketStreamM2S dataWidth metaOut]
packetize toMetaOut toHeader ps = L.concatMap (upConvert . prependHdr) bytePackets
 where
  prependHdr :: [PacketStreamM2S 1 metaIn] -> [PacketStreamM2S 1 metaOut]
  prependHdr fragments = hdr L.++ L.map (\f -> f{_meta = metaOut}) fragments
   where
    h = L.head fragments
    metaOut = toMetaOut (_meta h)
    hdr = L.map go (C.toList $ C.bitCoerce (toHeader (_meta h)))
    go byte = PacketStreamM2S (C.singleton byte) Nothing metaOut (_abort h)

  bytePackets :: [[PacketStreamM2S 1 metaIn]]
  bytePackets = downConvert . smearAbort <$> chunkByPacket ps

-- | Model of the generic `Protocols.PacketStream.packetizeFromDfC`.
packetizeFromDf ::
  forall
    (dataWidth :: C.Nat)
    (headerBytes :: C.Nat)
    (a :: C.Type)
    (header :: C.Type)
    (metaOut :: C.Type).
  (C.KnownNat dataWidth) =>
  (C.KnownNat headerBytes) =>
  (1 C.<= dataWidth) =>
  (1 C.<= headerBytes) =>
  (C.BitPack header) =>
  (C.BitSize header ~ headerBytes C.* 8) =>
  (a -> metaOut) ->
  (a -> header) ->
  [a] ->
  [PacketStreamM2S dataWidth metaOut]
packetizeFromDf toMetaOut toHeader = L.concatMap (upConvert . dfToPacket)
 where
  dfToPacket :: a -> [PacketStreamM2S 1 metaOut]
  dfToPacket d =
    fullPackets $
      L.map
        (\byte -> PacketStreamM2S (C.singleton byte) Nothing (toMetaOut d) False)
        (C.toList $ C.bitCoerce (toHeader d))

{- |
If set to @NoAbort@, packets will never contain a transfer with _abort set.
Otherwise, transfers of roughly 50% of the packets will randomly have _abort set.
-}
data AbortMode = Abort | NoAbort

{- |
Generate valid packets, i.e. packets of which all transfers carry the same
`_meta` and with all unenabled bytes in `_data` set to 0x00.
-}
genValidPackets ::
  forall (dataWidth :: C.Nat) (metaType :: C.Type).
  (1 C.<= dataWidth) =>
  (C.KnownNat dataWidth) =>
  (C.BitPack metaType) =>
  -- | The amount of packets to generate.
  Range Int ->
  -- | The amount of transfers with @_last = Nothing@ to generate per packet.
  --   This function will always generate an extra transfer per packet
  --   with @_last = Just i@.
  Range Int ->
  -- | If set to @NoAbort@, no generated packets will have `_abort` set in
  --   any of their transfers. Else, roughly 50% of packets will contain
  --   fragments with their `_abort` randomly set.
  AbortMode ->
  Gen [PacketStreamM2S dataWidth metaType]
genValidPackets pkts size Abort = concat <$> Gen.list pkts gen
 where
  gen = do
    abortPacket <- Gen.bool
    genValidPacket size (if abortPacket then Abort else NoAbort)
genValidPackets pkts size NoAbort =
  concat <$> Gen.list pkts (genValidPacket size NoAbort)

{- |
Generate a valid packet, i.e. a packet of which all transfers carry the same
`_meta` and with all unenabled bytes in `_data` set to 0x00.
-}
genValidPacket ::
  forall (dataWidth :: C.Nat) (metaType :: C.Type).
  (1 C.<= dataWidth) =>
  (C.KnownNat dataWidth) =>
  (C.BitPack metaType) =>
  -- | The amount of transfers with @_last = Nothing@ to generate.
  --   This function will always generate an extra transfer with @_last = Just i@.
  Range Int ->
  -- | If set to @NoAbort@, no transfers in the packet will have @_abort@ set.
  --   Else, they will randomly have @_abort@ set.
  AbortMode ->
  Gen [PacketStreamM2S dataWidth metaType]
genValidPacket size abortMode = do
  meta <- C.unpack <$> Gen.enumBounded
  transfers <- Gen.list size (genTransfer @dataWidth meta abortMode)
  lastTransfer <- genLastTransfer @dataWidth meta abortMode
  pure (transfers ++ [lastTransfer])

-- | Generate a single transfer which is not yet the end of a packet.
genTransfer ::
  forall (dataWidth :: C.Nat) (metaType :: C.Type).
  (1 C.<= dataWidth) =>
  (C.KnownNat dataWidth) =>
  -- | We need to use the same metadata
  --   for every transfer in a packet to satisfy the protocol
  --   invariant that metadata is constant for an entire packet.
  metaType ->
  -- | If set to @NoAbort@, hardcode @_abort@ to @False@. Else,
  --   randomly generate it.
  AbortMode ->
  Gen (PacketStreamM2S dataWidth metaType)
genTransfer meta abortMode =
  PacketStreamM2S
    <$> genVec Gen.enumBounded
    <*> Gen.constant Nothing
    <*> Gen.constant meta
    <*> case abortMode of
      Abort -> Gen.enumBounded
      NoAbort -> Gen.constant False

{- |
Generate the last transfer of a packet, i.e. a transfer with @_last@ set as @Just@.
All bytes which are not enabled are set to 0x00.
-}
genLastTransfer ::
  forall (dataWidth :: C.Nat) (metaType :: C.Type).
  (1 C.<= dataWidth) =>
  (C.KnownNat dataWidth) =>
  -- | We need to use the same metadata
  --   for every transfer in a packet to satisfy the protocol
  --   invariant that metadata is constant for an entire packet.
  metaType ->
  -- | If set to @NoAbort@, hardcode @_abort@ to @False@. Else,
  --   randomly generate it.
  AbortMode ->
  Gen (PacketStreamM2S dataWidth metaType)
genLastTransfer meta abortMode =
  setNull
    <$> ( PacketStreamM2S
            <$> genVec Gen.enumBounded
            <*> (Just <$> Gen.enumBounded)
            <*> Gen.constant meta
            <*> case abortMode of
              Abort -> Gen.enumBounded
              NoAbort -> Gen.constant False
        )
 where
  setNull transfer =
    let i = fromJust (_last transfer)
     in transfer
          { _data =
              fromJust
                ( Vec.fromList $
                    take (1 + fromIntegral i) (C.toList (_data transfer))
                      ++ replicate ((C.natToNum @dataWidth) - 1 - fromIntegral i) 0x00
                )
          }
