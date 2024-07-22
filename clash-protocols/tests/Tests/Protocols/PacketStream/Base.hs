{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards #-}

module Tests.Protocols.PacketStream.Base (
  chunkBy,
  chunkByPacket,
  smearAbort,
  chopBy,
  chunkToPacket,
  chopPacket,
  fullPackets,
  dropAbortedPackets,
  downConvert,
  upConvert,
  AbortMode (..),
  genValidPacket,
  genValidPackets,
  genVec,
) where

-- base
import qualified Data.List as L
import qualified Data.Maybe as M
import Prelude

-- clash
import qualified Clash.Prelude as C
import qualified Clash.Sized.Vector as Vec

-- hedgehog
import Hedgehog
import qualified Hedgehog.Gen as Gen

-- clash-protocols
import Protocols.PacketStream.Base

-- | Partition a list based on given function
chunkBy :: (a -> Bool) -> [a] -> [[a]]
chunkBy _ [] = []
chunkBy predicate list = L.filter (not . null) $ chunkByHelper predicate list []

-- Helper function to accumulate chunks
chunkByHelper :: (a -> Bool) -> [a] -> [a] -> [[a]]
chunkByHelper _ [] acc = [L.reverse acc]
chunkByHelper predicate (x : xs) acc
  | predicate x = L.reverse (x : acc) : chunkByHelper predicate xs []
  | otherwise = chunkByHelper predicate xs (x : acc)

-- | Partition a list of PacketStreams into complete packets
chunkByPacket :: [PacketStreamM2S n meta] -> [[PacketStreamM2S n meta]]
chunkByPacket = chunkBy (M.isJust . _last)

-- | Smear abort over the rest of a list of packets
smearAbort :: [PacketStreamM2S n meta] -> [PacketStreamM2S n meta]
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

-- | Merge a list of PacketStream 1 into a PacketStream n
chunkToPacket :: (C.KnownNat n) => [PacketStreamM2S 1 meta] -> PacketStreamM2S n meta
chunkToPacket l =
  PacketStreamM2S
    { _last =
        if M.isJust $ _last $ L.last l then M.Just (fromIntegral $ L.length l - 1) else Nothing
    , _abort = any _abort l
    , _meta = _meta $ L.head l
    , _data = foldr ((C.+>>) . C.head . _data) (C.repeat 0) l
    }

-- | Split a PacketStream n into a list of PacketStream 1
chopPacket ::
  forall n meta.
  (1 C.<= n) =>
  (C.KnownNat n) =>
  PacketStreamM2S n meta ->
  [PacketStreamM2S 1 meta]
chopPacket PacketStreamM2S{..} = packets
 where
  lasts = case _last of
    Nothing -> repeat Nothing
    Just in' -> replicate (fromIntegral in') Nothing ++ [Just (0 :: C.Index 1)]

  datas = case _last of
    Nothing -> C.toList _data
    Just in' -> take (fromIntegral in' + 1) $ C.toList _data

  packets = (\(idx, dat) -> PacketStreamM2S (pure dat) idx _meta _abort) <$> zip lasts datas

-- | Set the _last of the last element of a list of PacketStreams to 0
fullPackets :: (C.KnownNat n) => [PacketStreamM2S n meta] -> [PacketStreamM2S n meta]
fullPackets [] = []
fullPackets fragments =
  let lastFragment = (last fragments){_last = Just 0}
   in init fragments ++ [lastFragment]

-- | Drops packets if one of the transfers in the packet has the abort flag set
dropAbortedPackets :: [PacketStreamM2S n meta] -> [PacketStreamM2S n meta]
dropAbortedPackets packets = concat $ filter (not . any _abort) (chunkByPacket packets)

-- | Split a list of PacketStream n into a list of PacketStream 1
downConvert ::
  forall n meta.
  (1 C.<= n) =>
  (C.KnownNat n) =>
  [PacketStreamM2S n meta] ->
  [PacketStreamM2S 1 meta]
downConvert = concatMap chopPacket

-- | Merge a list of PacketStream 1 into a list of PacketStream n
upConvert ::
  forall n meta.
  (1 C.<= n) =>
  (C.KnownNat n) =>
  [PacketStreamM2S 1 meta] ->
  [PacketStreamM2S n meta]
upConvert packets = chunkToPacket <$> chopBy (C.natToNum @n) packets

{- | If set to @NoAbort@, packets will never contain a transfer with _abort set.
  Otherwise, transfers of roughly 50% of the packets will randomly have _abort set.
-}
data AbortMode = Abort | NoAbort

{- | Generate valid packets, i.e. packets of which all transfers carry the same
  @_meta@ and with all unenabled bytes in @_data@ set to 0x00.
-}
genValidPackets ::
  forall (dataWidth :: C.Nat) (metaType :: C.Type).
  (1 C.<= dataWidth) =>
  (C.KnownNat dataWidth) =>
  (C.BitPack metaType) =>
  -- | The amount of packets to generate.
  Range Int ->
  -- | The amount of transfers to generate.
  Range Int ->
  -- | If set to @NoAbort@, no generated packets will have @_abort@ set in
  --   any of their transfers. Else, roughly 50% of packets will contain
  --   fragments with their @_abort@ randomly set.
  AbortMode ->
  Gen [PacketStreamM2S dataWidth metaType]
genValidPackets r1 r2 Abort = concat <$> Gen.list r1 x
 where
  x = do
    abortPacket <- Gen.bool
    genValidPacket r2 (if abortPacket then Abort else NoAbort)
genValidPackets r1 r2 NoAbort = concat <$> Gen.list r1 (genValidPacket r2 NoAbort)

{- | Generate a valid packet, i.e. a packet of which all transfers carry the same
  @_meta@ and with all unenabled bytes in @_data@ set to 0x00.
-}
genValidPacket ::
  forall (dataWidth :: C.Nat) (metaType :: C.Type).
  (1 C.<= dataWidth) =>
  (C.KnownNat dataWidth) =>
  (C.BitPack metaType) =>
  -- | The amount of transfers to generate.
  Range Int ->
  -- | If set to @NoAbort@, no transfers in the packet will have @_abort@ set.
  --   Else, they will randomly have @_abort@ set.
  AbortMode ->
  Gen [PacketStreamM2S dataWidth metaType]
genValidPacket r abortMode = do
  meta <- C.unpack <$> Gen.enumBounded
  transfers <- Gen.list r (genTransfer @dataWidth meta abortMode)
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

{- | Generate the last transfer of a packet, i.e. a transfer with @_last@ set as @Just@.
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
    let i = M.fromJust (_last transfer)
     in transfer
          { _data =
              M.fromJust
                ( Vec.fromList $
                    take (1 + fromIntegral i) (C.toList (_data transfer))
                      ++ replicate ((C.natToNum @dataWidth) - 1 - fromIntegral i) 0x00
                )
          }

-- | Randomly generate a vector of length @n@.
genVec ::
  forall (n :: C.Nat) (a :: C.Type).
  (C.KnownNat n, 1 C.<= n) =>
  Gen a ->
  Gen (C.Vec n a)
genVec gen = sequence (C.repeat gen)
