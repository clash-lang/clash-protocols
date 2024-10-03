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
  chopBy,
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
  dropTailModel,
  packetizerModel,
  packetizeFromDfModel,

  -- * Hedgehog generators
  AbortMode (..),
  genValidPacket,
  genPackets,
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
chunkToPacket xs =
  PacketStreamM2S
    { _last =
        (\i -> let l = fromIntegral (L.length xs) in if i == 0 then l - 1 else l)
          <$> _last lastTransfer
    , _abort = any _abort xs
    , _meta = _meta lastTransfer
    , _data = foldr ((C.+>>) . C.head . _data) (C.repeat 0x00) xs
    }
 where
  lastTransfer = L.last xs

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
    Just size ->
      if size == 0
        then [Just 0]
        else replicate (fromIntegral size - 1) Nothing ++ [Just (1 :: C.Index 2)]

  datas = case _last of
    Nothing -> C.toList _data
    Just size -> take (max 1 (fromIntegral size)) $ C.toList _data

  packets =
    ( \(size, dat) ->
        PacketStreamM2S (pure dat) size _meta _abort
    )
      <$> zip lasts datas

-- | Set `_last` of the last transfer in the list to @Just 1@
fullPackets ::
  (C.KnownNat dataWidth) =>
  [PacketStreamM2S dataWidth meta] ->
  [PacketStreamM2S dataWidth meta]
fullPackets [] = []
fullPackets fragments =
  let lastFragment = (last fragments){_last = Just 1}
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
Merges a list of `PacketStream` transfers with data width @dataWidth@ into
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
  parseHdr (hdrF, fwdF) = fmap (\f -> f{_meta = metaOut}) fwdF'
   where
    fwdF' = case fwdF of
      [] -> [PacketStreamM2S (Vec.singleton 0x00) (Just 0) (C.errorX "u") (_abort (last hdrF))]
      _ -> fwdF

    hdr = C.bitCoerce $ Vec.unsafeFromList @headerBytes $ _data <$> hdrF
    metaOut = toMetaOut hdr (_meta $ L.head fwdF)

  bytePackets :: [[PacketStreamM2S 1 metaIn]]
  bytePackets =
    L.filter
      ( \fs ->
          let len' = L.length fs
           in len' > hdrbytes || len' == hdrbytes && _last (last fs) == Just 1
      )
      $ downConvert . smearAbort <$> chunkByPacket ps

  parsedPackets :: [[PacketStreamM2S 1 metaOut]]
  parsedPackets = L.map go bytePackets

  go = parseHdr . L.splitAt hdrbytes

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
      ( \pkt ->
          L.length pkt > C.natToNum @headerBytes
            || L.length pkt == C.natToNum @headerBytes && _last (last pkt) == Just 1
      )
      (chunkByPacket $ downConvert (dropAbortedPackets ps))

-- | Model of 'Protocols.PacketStream.dropTailC'.
dropTailModel ::
  forall dataWidth meta n.
  (C.KnownNat dataWidth) =>
  (1 C.<= dataWidth) =>
  (1 C.<= n) =>
  C.SNat n ->
  [PacketStreamM2S dataWidth meta] ->
  [PacketStreamM2S dataWidth meta]
dropTailModel C.SNat packets = L.concatMap go (chunkByPacket packets)
 where
  go :: [PacketStreamM2S dataWidth meta] -> [PacketStreamM2S dataWidth meta]
  go packet =
    upConvert $
      L.init trimmed L.++ [(L.last trimmed){_last = Just 1, _abort = aborted}]
   where
    aborted = L.any _abort packet
    bytePkts = downConvert packet
    trimmed = L.take (L.length bytePkts - C.natToNum @n) bytePkts

-- | Model of the generic `Protocols.PacketStream.packetizerC`.
packetizerModel ::
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
packetizerModel toMetaOut toHeader ps = L.concatMap (upConvert . prependHdr) bytePackets
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
packetizeFromDfModel ::
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
packetizeFromDfModel toMetaOut toHeader = L.concatMap (upConvert . dfToPacket)
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
Generate packets with a user-supplied generator.
-}
genPackets ::
  forall (dataWidth :: C.Nat) (meta :: C.Type).
  (1 C.<= dataWidth) =>
  (C.KnownNat dataWidth) =>
  -- | The amount of packets to generate.
  Range Int ->
  -- | If set to @NoAbort@, always pass @NoAbort@ to the packet generator.
  --   Else, pass @Abort@ to roughly 50% of the packet generators.
  AbortMode ->
  -- | Packet generator.
  (AbortMode -> Gen [PacketStreamM2S dataWidth meta]) ->
  Gen [PacketStreamM2S dataWidth meta]
genPackets pkts Abort pktGen =
  concat
    <$> Gen.list
      pkts
      (Gen.choice [pktGen Abort, pktGen NoAbort])
genPackets pkts NoAbort pktGen =
  concat
    <$> Gen.list
      pkts
      (pktGen NoAbort)

{- |
Generate a valid packet, i.e. a packet of which all transfers carry the same
`_meta` and with all unenabled bytes in `_data` set to 0x00.
-}
genValidPacket ::
  forall (dataWidth :: C.Nat) (meta :: C.Type).
  (1 C.<= dataWidth) =>
  (C.KnownNat dataWidth) =>
  -- | Generator for the metadata.
  Gen meta ->
  -- | The amount of transfers with @_last = Nothing@ to generate.
  --   This function will always generate an extra transfer with @_last = Just i@.
  Range Int ->
  -- | If set to @NoAbort@, no transfers in the packet will have @_abort@ set.
  --   Else, each transfer has a 10% chance to have @_abort@ set.
  AbortMode ->
  Gen [PacketStreamM2S dataWidth meta]
genValidPacket metaGen size abortMode = do
  meta <- metaGen
  transfers <- Gen.list size (genTransfer @dataWidth meta abortMode)
  lastTransfer <- genLastTransfer @dataWidth meta abortMode
  pure (transfers ++ [lastTransfer])

-- | Generate a single transfer which is not yet the end of a packet.
genTransfer ::
  forall (dataWidth :: C.Nat) (meta :: C.Type).
  (1 C.<= dataWidth) =>
  (C.KnownNat dataWidth) =>
  -- | We need to use the same metadata
  --   for every transfer in a packet to satisfy the protocol
  --   invariant that metadata is constant for an entire packet.
  meta ->
  -- | If set to @NoAbort@, hardcode @_abort@ to @False@. Else,
  --   there is a 10% chance for it to be set.
  AbortMode ->
  Gen (PacketStreamM2S dataWidth meta)
genTransfer meta abortMode =
  PacketStreamM2S
    <$> genVec Gen.enumBounded
    <*> Gen.constant Nothing
    <*> Gen.constant meta
    <*> case abortMode of
      Abort ->
        Gen.frequency
          [ (90, Gen.constant False)
          , (10, Gen.constant True)
          ]
      NoAbort -> Gen.constant False

{- |
Generate the last transfer of a packet, i.e. a transfer with @_last@ set as @Just@.
All bytes which are not enabled are set to 0x00.
-}
genLastTransfer ::
  forall (dataWidth :: C.Nat) (meta :: C.Type).
  (1 C.<= dataWidth) =>
  (C.KnownNat dataWidth) =>
  -- | We need to use the same metadata
  --   for every transfer in a packet to satisfy the protocol
  --   invariant that metadata is constant for an entire packet.
  meta ->
  -- | If set to @NoAbort@, hardcode @_abort@ to @False@. Else,
  --   randomly generate it.
  AbortMode ->
  Gen (PacketStreamM2S dataWidth meta)
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
                    take (fromIntegral i) (C.toList (_data transfer))
                      ++ replicate ((C.natToNum @dataWidth) - fromIntegral i) 0x00
                )
          }
