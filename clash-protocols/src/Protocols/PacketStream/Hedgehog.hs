{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NoImplicitPrelude #-}

{- |
Copyright  :  (C) 2024, QBayLogic B.V.
License    :  BSD2 (see the file LICENSE)
Maintainer :  QBayLogic B.V. <devops@qbaylogic.com>

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
  PacketOptions (..),
  defPacketOptions,
  genValidPacket,
  genPackets,
) where

import Clash.Hedgehog.Sized.Vector (genVec)
import Clash.Prelude
import qualified Clash.Sized.Vector as Vec

import qualified Data.List as L
import Data.Maybe (fromJust, isJust)

import Hedgehog (Gen, Range)
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

import Protocols.PacketStream.Base

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
chopBy n xs = as : chopBy n bs where (as, bs) = L.splitAt n xs

{- |
Merge a list of `PacketStream` transfers with data width @1@ to
a single `PacketStream` transfer with data width @dataWidth@.
-}
chunkToPacket ::
  (KnownNat dataWidth) =>
  [PacketStreamM2S 1 meta] ->
  PacketStreamM2S dataWidth meta
chunkToPacket xs =
  PacketStreamM2S
    { _last =
        (\i -> let l = fromIntegral (L.length xs) in if i == 0 then l - 1 else l)
          <$> _last lastTransfer
    , _abort = any _abort xs
    , _meta = _meta lastTransfer
    , _data = L.foldr ((+>>) . head . _data) (repeat (nullByte "chunkToPacket")) xs
    }
 where
  lastTransfer = L.last xs

{- |
Split a single `PacketStream` transfer with data width @dataWidth@ to
a list of `PacketStream` transfers with data width @1@.
-}
chopPacket ::
  forall dataWidth meta.
  (1 <= dataWidth) =>
  (KnownNat dataWidth) =>
  PacketStreamM2S dataWidth meta ->
  [PacketStreamM2S 1 meta]
chopPacket PacketStreamM2S{..} = packets
 where
  lasts = case _last of
    Nothing -> L.repeat Nothing
    Just size ->
      if size == 0
        then [Just 0]
        else L.replicate (fromIntegral size - 1) Nothing L.++ [Just (1 :: Index 2)]

  datas = case _last of
    Nothing -> toList _data
    Just size -> L.take (max 1 (fromIntegral size)) $ toList _data

  packets =
    ( \(size, dat) ->
        PacketStreamM2S (pure dat) size _meta _abort
    )
      <$> L.zip lasts datas

-- | Set `_last` of the last transfer in the list to @Just 1@
fullPackets ::
  (KnownNat dataWidth) =>
  [PacketStreamM2S dataWidth meta] ->
  [PacketStreamM2S dataWidth meta]
fullPackets [] = []
fullPackets fragments =
  let lastFragment = (L.last fragments){_last = Just 1}
   in L.init fragments L.++ [lastFragment]

-- | Drops packets if one of the transfers in the packet has `_abort` set.
dropAbortedPackets ::
  [PacketStreamM2S dataWidth meta] ->
  [PacketStreamM2S dataWidth meta]
dropAbortedPackets packets = L.concat $ L.filter (not . any _abort) (chunkByPacket packets)

{- |
Splits a list of `PacketStream` transfers with data width @1@ into
a list of `PacketStream` transfers with data width @dataWidth@
-}
downConvert ::
  forall dataWidth meta.
  (1 <= dataWidth) =>
  (KnownNat dataWidth) =>
  [PacketStreamM2S dataWidth meta] ->
  [PacketStreamM2S 1 meta]
downConvert = L.concatMap chopPacket

{- |
Merges a list of `PacketStream` transfers with data width @dataWidth@ into
a list of `PacketStream` transfers with data width @1@
-}
upConvert ::
  forall dataWidth meta.
  (1 <= dataWidth) =>
  (KnownNat dataWidth) =>
  [PacketStreamM2S 1 meta] ->
  [PacketStreamM2S dataWidth meta]
upConvert packets =
  L.map
    chunkToPacket
    (chunkByPacket packets >>= chopBy (natToNum @dataWidth))

-- | Model of the generic `Protocols.PacketStream.depacketizerC`.
depacketizerModel ::
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
  (NFDataX metaIn) =>
  (BitPack header) =>
  (BitSize header ~ headerBytes * 8) =>
  (header -> metaIn -> metaOut) ->
  [PacketStreamM2S dataWidth metaIn] ->
  [PacketStreamM2S dataWidth metaOut]
depacketizerModel toMetaOut ps = L.concat dataWidthPackets
 where
  hdrBytes = natToNum @headerBytes

  parseHdr ::
    ([PacketStreamM2S 1 metaIn], [PacketStreamM2S 1 metaIn]) ->
    [PacketStreamM2S 1 metaOut]
  parseHdr (hdrF, fwdF) = fmap (\f -> f{_meta = metaOut}) fwdF'
   where
    fwdF' = case fwdF of
      [] ->
        [ PacketStreamM2S
            (Vec.singleton (nullByte "depacketizerModel"))
            (Just 0)
            (deepErrorX "depacketizerModel: should be replaced")
            (_abort (L.last hdrF))
        ]
      _ -> fwdF

    hdr = bitCoerce $ Vec.unsafeFromList @headerBytes $ _data <$> hdrF
    metaIn = case hdrF of
      [] ->
        -- There are @headerBytes@ packets in this list, and (1 <= headerBytes)
        error "depacketizerModel: absurd"
      (hdrF0 : _) -> _meta hdrF0
    metaOut = toMetaOut hdr metaIn

  bytePackets :: [[PacketStreamM2S 1 metaIn]]
  bytePackets =
    L.filter
      ( \fs ->
          let len' = L.length fs
           in len' > hdrBytes || len' == hdrBytes && _last (L.last fs) == Just 1
      )
      $ downConvert
      . smearAbort
      <$> chunkByPacket ps

  parsedPackets :: [[PacketStreamM2S 1 metaOut]]
  parsedPackets = L.map go bytePackets

  go = parseHdr . L.splitAt hdrBytes

  dataWidthPackets :: [[PacketStreamM2S dataWidth metaOut]]
  dataWidthPackets = L.map upConvert parsedPackets

-- | Model of the generic `Protocols.PacketStream.depacketizeToDfC`.
depacketizeToDfModel ::
  forall
    (dataWidth :: Nat)
    (headerBytes :: Nat)
    (a :: Type)
    (header :: Type)
    (metaIn :: Type).
  (KnownNat dataWidth) =>
  (KnownNat headerBytes) =>
  (1 <= dataWidth) =>
  (1 <= headerBytes) =>
  (BitPack header) =>
  (BitSize header ~ headerBytes * 8) =>
  (header -> metaIn -> a) ->
  [PacketStreamM2S dataWidth metaIn] ->
  [a]
depacketizeToDfModel toOut ps = L.map parseHdr bytePackets
 where
  parseHdr :: [PacketStreamM2S 1 metaIn] -> a
  parseHdr [] =
    -- There are at least @headerBytes@ packets in this list, and
    -- (1 <= headerBytes)
    error "depacketizeToDfModel: absurd"
  parseHdr hdrF@(hdrF0 : _) =
    toOut
      (bitCoerce $ Vec.unsafeFromList $ L.map _data hdrF)
      (_meta hdrF0)

  bytePackets :: [[PacketStreamM2S 1 metaIn]]
  bytePackets =
    L.filter
      ( \pkt ->
          (L.length pkt > natToNum @headerBytes)
            || (L.length pkt == natToNum @headerBytes && _last (L.last pkt) == Just 1)
      )
      (chunkByPacket $ downConvert (dropAbortedPackets ps))

-- | Model of 'Protocols.PacketStream.dropTailC'.
dropTailModel ::
  forall dataWidth meta n.
  (KnownNat dataWidth) =>
  (1 <= dataWidth) =>
  (1 <= n) =>
  SNat n ->
  [PacketStreamM2S dataWidth meta] ->
  [PacketStreamM2S dataWidth meta]
dropTailModel SNat packets = L.concatMap go (chunkByPacket packets)
 where
  go :: [PacketStreamM2S dataWidth meta] -> [PacketStreamM2S dataWidth meta]
  go packet =
    upConvert
      $ L.init trimmed
      L.++ [(L.last trimmed){_last = _last $ L.last bytePkts, _abort = aborted}]
   where
    aborted = L.any _abort packet
    bytePkts = downConvert packet
    trimmed = L.take (L.length bytePkts - natToNum @n) bytePkts

-- | Model of the generic `Protocols.PacketStream.packetizerC`.
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
  prependHdr [] =
    -- 'chunkBy' filters empty lists, so all elements of bytePackets are
    -- guaranteed to be non-null
    error "packetizerModel: Unreachable code"
  prependHdr fragments@(h : _) =
    hdr L.++ L.map (\f -> f{_meta = metaOut}) fragments
   where
    metaOut = toMetaOut (_meta h)
    hdr = L.map go (toList $ bitCoerce (toHeader (_meta h)))
    go byte = PacketStreamM2S (singleton byte) Nothing metaOut (_abort h)

  bytePackets :: [[PacketStreamM2S 1 metaIn]]
  bytePackets = downConvert . smearAbort <$> chunkByPacket ps

-- | Model of the generic `Protocols.PacketStream.packetizeFromDfC`.
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
packetizeFromDfModel toMetaOut toHeader = L.concatMap (upConvert . dfToPacket)
 where
  dfToPacket :: a -> [PacketStreamM2S 1 metaOut]
  dfToPacket d =
    fullPackets
      $ L.map
        (\byte -> PacketStreamM2S (singleton byte) Nothing (toMetaOut d) False)
        (toList $ bitCoerce (toHeader d))

-- | Abort generation options for packet generation.
data AbortMode
  = Abort
      { amPacketGen :: Gen Bool
      -- ^ Determines the chance to generate aborted fragments in a packet.
      , amTransferGen :: Gen Bool
      -- ^ Determines the frequency of aborted fragments in a packet.
      }
  | NoAbort

-- | Various configuration options for packet generation.
data PacketOptions = PacketOptions
  { poAllowEmptyPackets :: Bool
  -- ^ Whether to allow the generation of zero-byte packets.
  , poAllowTrailingEmpty :: Bool
  -- ^ Whether to allow the generation of trailing zero-byte transfers.
  , poAbortMode :: AbortMode
  -- ^ If set to @NoAbort@, no transfers in the packet will have '_abort' set.
  -- Else, randomly generate them according to some distribution. See 'AbortMode'.
  }

{- |
Default packet generation options:

- Allow the generation of a zero-byte packet;
- Allow the generation of a trailing zero-byte transfer;
- 50% chance to generate aborted transfers. If aborts are generated, 10% of
  transfers will be aborted.
-}
defPacketOptions :: PacketOptions
defPacketOptions =
  PacketOptions
    { poAllowEmptyPackets = True
    , poAllowTrailingEmpty = True
    , poAbortMode =
        Abort
          { amPacketGen = Gen.enumBounded
          , amTransferGen =
              Gen.frequency
                [ (90, Gen.constant False)
                , (10, Gen.constant True)
                ]
          }
    }

{- |
Generate packets with a user-supplied generator and a linear range.
-}
genPackets ::
  forall (dataWidth :: Nat) (meta :: Type).
  (1 <= dataWidth) =>
  (KnownNat dataWidth) =>
  -- | Minimum amount of packets to generate.
  Int ->
  -- | Maximum amount of packets to generate.
  Int ->
  -- | Packet generator.
  Gen [PacketStreamM2S dataWidth meta] ->
  Gen [PacketStreamM2S dataWidth meta]
genPackets minB maxB pktGen = L.concat <$> Gen.list (Range.linear minB maxB) pktGen
{-# INLINE genPackets #-}

{- |
Generate a valid packet, i.e. a packet of which all transfers carry the same
`_meta` and with all bytes in `_data` that are not enabled set to 0x00.
-}
genValidPacket ::
  forall (dataWidth :: Nat) (meta :: Type).
  (1 <= dataWidth) =>
  (KnownNat dataWidth) =>
  -- | Configuration options for packet generation.
  PacketOptions ->
  -- | Generator for the metadata.
  Gen meta ->
  -- | The amount of transfers with @_last = Nothing@ to generate.
  --   This function will always generate an extra transfer with @_last = Just i@.
  Range Int ->
  Gen [PacketStreamM2S dataWidth meta]
genValidPacket PacketOptions{..} metaGen size =
  let
    abortGen = case poAbortMode of
      NoAbort -> Gen.constant False
      Abort pktGen transferGen -> do
        allowAborts <- pktGen
        (if allowAborts then transferGen else Gen.constant False)
   in
    do
      meta <- metaGen
      transfers <- Gen.list size (genTransfer meta abortGen)
      lastTransfer <-
        genLastTransfer
          meta
          ( (null transfers && poAllowEmptyPackets)
              || (not (null transfers) && poAllowTrailingEmpty)
          )
          abortGen
      pure (transfers L.++ [lastTransfer])

-- | Generate a single transfer which is not yet the end of a packet.
genTransfer ::
  forall (dataWidth :: Nat) (meta :: Type).
  (1 <= dataWidth) =>
  (KnownNat dataWidth) =>
  -- | We need to use the same metadata
  --   for every transfer in a packet to satisfy the protocol
  --   invariant that metadata is constant for an entire packet.
  meta ->
  -- | Whether to set '_abort'.
  Gen Bool ->
  Gen (PacketStreamM2S dataWidth meta)
genTransfer meta abortGen =
  PacketStreamM2S
    <$> genVec Gen.enumBounded
    <*> Gen.constant Nothing
    <*> Gen.constant meta
    <*> abortGen

{- |
Generate the last transfer of a packet, i.e. a transfer with @_last@ set as @Just@.
All bytes which are not enabled are forced /undefined/.
-}
genLastTransfer ::
  forall (dataWidth :: Nat) (meta :: Type).
  (1 <= dataWidth) =>
  (KnownNat dataWidth) =>
  -- | We need to use the same metadata
  --   for every transfer in a packet to satisfy the protocol
  --   invariant that metadata is constant for an entire packet.
  meta ->
  -- | Whether we are allowed to generate a 0-byte transfer.
  Bool ->
  -- | Whether to set '_abort'.
  Gen Bool ->
  Gen (PacketStreamM2S dataWidth meta)
genLastTransfer meta allowEmpty abortGen =
  setNull
    <$> ( PacketStreamM2S
            <$> genVec Gen.enumBounded
            <*> (Just <$> Gen.enum (if allowEmpty then 0 else 1) maxBound)
            <*> Gen.constant meta
            <*> abortGen
        )

setNull ::
  forall (dataWidth :: Nat) (meta :: Type).
  (KnownNat dataWidth) =>
  PacketStreamM2S dataWidth meta ->
  PacketStreamM2S dataWidth meta
setNull transfer =
  let i = fromJust (_last transfer)
   in transfer
        { _data =
            fromJust
              ( Vec.fromList
                  $ L.take (fromIntegral i) (toList (_data transfer))
                  L.++ L.replicate ((natToNum @dataWidth) - fromIntegral i) (nullByte "setNull")
              )
        }
