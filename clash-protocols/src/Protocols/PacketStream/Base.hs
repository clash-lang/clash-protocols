{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_HADDOCK hide #-}

{- |
Definitions and instances of the PacketStream protocol
-}
module Protocols.PacketStream.Base (
  PacketStreamM2S (..),
  PacketStreamS2M (..),
  PacketStream,
  abortOnBackPressureC,
  unsafeToPacketStream,
  fromPacketStream,
  forceResetSanity,
  filterMetaS,
  filterMeta,
  mapMetaS,
  mapMeta,
  zeroOutInvalidBytesC,
) where

import Clash.Prelude hiding (sample)
import qualified Prelude as P

import qualified Protocols.Df as Df
import qualified Protocols.DfConv as DfConv
import Protocols.Hedgehog.Internal
import Protocols.Internal

import Control.DeepSeq (NFData)
import Data.Coerce (coerce)
import qualified Data.Maybe as Maybe
import Data.Proxy

{- |
Data sent from manager to subordinate.

Heavily inspired by the M2S data of AMBA AXI4-Stream, but simplified:

- @_tdata@ is moved into @_data@, which serves the exact same purpose: the actual
  data of the transfer.
- @_tkeep@ is changed to `_last`.
- @_tstrb@ is removed as there are no position bytes.
- @_tid@ is removed, because packets may not be interrupted by other packets.
- @_tdest@ is moved into `_meta`.
- @_tuser@ is moved into `_meta`.
- @_tvalid@ is modeled by wrapping this type into a @Maybe@.
-}
data PacketStreamM2S (dataWidth :: Nat) (meta :: Type) = PacketStreamM2S
  { _data :: Vec dataWidth (BitVector 8)
  -- ^ The bytes to be transmitted.
  , _last :: Maybe (Index dataWidth)
  -- ^ If this is @Just@ then it signals that this transfer
  --   is the end of a packet and contains the index of the last valid byte in `_data`.
  --   If it is @Nothing@ then this transfer is not yet the end of a packet and all
  --   bytes are valid. This implies that no null bytes are allowed in the middle of
  --   a packet, only after a packet.
  , _meta :: meta
  -- ^ Metadata of a packet. Must be constant during a packet.
  , _abort :: Bool
  -- ^ Iff true, the packet corresponding to this transfer is invalid. The subordinate
  --   must either drop the packet or forward the `_abort`.
  }
  deriving (Eq, Generic, ShowX, Show, NFData, Bundle, Functor)

{- |
Data sent from the subordinate to manager.

The only information transmitted is whether the subordinate is ready to receive data.
-}
newtype PacketStreamS2M = PacketStreamS2M
  { _ready :: Bool
  -- ^ Iff True, the subordinate is ready to receive data.
  }
  deriving (Eq, Generic, ShowX, Show, NFData, Bundle, NFDataX)

{- |
Simple valid-ready streaming protocol for transferring packets between components.

Invariants:

1. A manager must not check the `Bwd` channel when it is sending @Nothing@ over the `Fwd` channel.
2. A manager must keep sending the same data until the subordinate has acknowledged it, i.e. upon observing `_ready` as @True@.
3. A manager must keep the metadata (`_meta`) of an entire packet it sends constant.
4. A subordinate which receives a transfer with `_abort` asserted must either forward this `_abort` or drop the packet.
5. A packet may not be interrupted by another packet.
6. All bytes in `_data` which are not enabled must be 0x00.
-}
data PacketStream (dom :: Domain) (dataWidth :: Nat) (meta :: Type)

deriving instance
  (KnownNat dataWidth, NFDataX meta) =>
  NFDataX (PacketStreamM2S dataWidth meta)

instance Protocol (PacketStream dom dataWidth meta) where
  type
    Fwd (PacketStream dom dataWidth meta) =
      Signal dom (Maybe (PacketStreamM2S dataWidth meta))
  type Bwd (PacketStream dom dataWidth meta) = Signal dom PacketStreamS2M

instance IdleCircuit (PacketStream dom dataWidth meta) where
  idleBwd _ = pure (PacketStreamS2M False)
  idleFwd _ = pure Nothing

instance Backpressure (PacketStream dom dataWidth meta) where
  boolsToBwd _ = fromList_lazy . fmap PacketStreamS2M

instance DfConv.DfConv (PacketStream dom dataWidth meta) where
  type Dom (PacketStream dom dataWidth meta) = dom
  type FwdPayload (PacketStream dom dataWidth meta) = PacketStreamM2S dataWidth meta

  toDfCircuit _ = fromSignals go
   where
    go (fwdIn, bwdIn) =
      ( (fmap coerce bwdIn, pure undefined)
      , Df.dataToMaybe <$> P.fst fwdIn
      )

  fromDfCircuit _ = fromSignals go
   where
    go (fwdIn, bwdIn) =
      ( coerce <$> P.fst bwdIn
      , (fmap Df.maybeToData fwdIn, pure undefined)
      )

instance
  (KnownDomain dom) =>
  Simulate (PacketStream dom dataWidth meta)
  where
  type
    SimulateFwdType (PacketStream dom dataWidth meta) =
      [Maybe (PacketStreamM2S dataWidth meta)]
  type SimulateBwdType (PacketStream dom dataWidth meta) = [PacketStreamS2M]
  type SimulateChannels (PacketStream dom dataWidth meta) = 1

  simToSigFwd _ = fromList_lazy
  simToSigBwd _ = fromList_lazy
  sigToSimFwd _ s = sample_lazy s
  sigToSimBwd _ s = sample_lazy s

  stallC conf (head -> (stallAck, stalls)) =
    withClockResetEnable clockGen resetGen enableGen
      $ DfConv.stall Proxy Proxy conf stallAck stalls

instance
  (KnownDomain dom) =>
  Drivable (PacketStream dom dataWidth meta)
  where
  type
    ExpectType (PacketStream dom dataWidth meta) =
      [PacketStreamM2S dataWidth meta]

  toSimulateType Proxy = fmap Just
  fromSimulateType Proxy = Maybe.catMaybes

  driveC conf vals =
    withClockResetEnable clockGen resetGen enableGen
      $ DfConv.drive Proxy conf vals
  sampleC conf ckt =
    withClockResetEnable clockGen resetGen enableGen
      $ DfConv.sample Proxy conf ckt

instance
  ( KnownNat dataWidth
  , NFDataX meta
  , NFData meta
  , ShowX meta
  , Show meta
  , Eq meta
  , KnownDomain dom
  ) =>
  Test (PacketStream dom dataWidth meta)
  where
  expectN Proxy options sampled =
    expectN (Proxy @(Df.Df dom _)) options
      $ Df.maybeToData
      <$> sampled

-- | Circuit to convert a CSignal into a PacketStream. This is unsafe, because it drops backpressure.
unsafeToPacketStream ::
  forall dom dataWidth meta.
  Circuit
    (CSignal dom (Maybe (PacketStreamM2S dataWidth meta)))
    (PacketStream dom dataWidth meta)
unsafeToPacketStream = Circuit (\(fwdInS, _) -> (pure (), fwdInS))

-- | Converts a PacketStream into a CSignal.
fromPacketStream ::
  forall dom dataWidth meta.
  (HiddenClockResetEnable dom) =>
  Circuit
    (PacketStream dom dataWidth meta)
    (CSignal dom (Maybe (PacketStreamM2S dataWidth meta)))
fromPacketStream = forceResetSanity |> Circuit (\(fwdIn, _) -> (pure (PacketStreamS2M True), fwdIn))

-- | A circuit that sets `_abort` upon backpressure from the forward circuit.
abortOnBackPressureC ::
  forall (dom :: Domain) (dataWidth :: Nat) (meta :: Type).
  (HiddenClockResetEnable dom) =>
  (KnownNat dataWidth) =>
  (NFDataX meta) =>
  Circuit
    (CSignal dom (Maybe (PacketStreamM2S dataWidth meta)))
    (PacketStream dom dataWidth meta)
abortOnBackPressureC = Circuit $ \(fwdInS, bwdInS) -> (pure (), go <$> bundle (fwdInS, bwdInS))
 where
  go (fwdIn, bwdIn) = fmap (\pkt -> pkt{_abort = _abort pkt || not (_ready bwdIn)}) fwdIn

{- |
Force a /nack/ on the backward channel and /Nothing/ on the forward
channel if reset is asserted.
-}
forceResetSanity ::
  forall dom dataWidth meta.
  (HiddenClockResetEnable dom) =>
  Circuit (PacketStream dom dataWidth meta) (PacketStream dom dataWidth meta)
forceResetSanity = forceResetSanityGeneric

{- |
Filter a packet stream based on its metadata,
with the predicate wrapped in a @Signal@.
-}
filterMetaS ::
  -- | Predicate which specifies whether to keep a fragment based on its metadata,
  --   wrapped in a @Signal@
  Signal dom (meta -> Bool) ->
  Circuit (PacketStream dom dataWidth meta) (PacketStream dom dataWidth meta)
filterMetaS pS = Circuit $ \(fwdIn, bwdIn) -> unbundle (go <$> bundle (fwdIn, bwdIn, pS))
 where
  go (Nothing, bwdIn, _) = (bwdIn, Nothing)
  go (Just inPkt, bwdIn, predicate)
    | predicate (_meta inPkt) = (bwdIn, Just inPkt)
    | otherwise = (PacketStreamS2M True, Nothing)

-- | Filter a packet stream based on its metadata.
filterMeta ::
  -- | Predicate which specifies whether to keep a fragment based on its metadata
  (meta -> Bool) ->
  Circuit (PacketStream dom dataWidth meta) (PacketStream dom dataWidth meta)
filterMeta p = filterMetaS (pure p)

{- |
Map a function on the metadata of a packet stream,
with the function wrapped in a @Signal@.
-}
mapMetaS ::
  -- | Function to apply on the metadata, wrapped in a @Signal@
  Signal dom (metaIn -> metaOut) ->
  Circuit (PacketStream dom dataWidth metaIn) (PacketStream dom dataWidth metaOut)
mapMetaS fS = Circuit $ \(fwdIn, bwdIn) -> (bwdIn, go <$> bundle (fwdIn, fS))
 where
  go (inp, f) = (\inPkt -> inPkt{_meta = f (_meta inPkt)}) <$> inp

-- | Map a function on the metadata of a packet stream.
mapMeta ::
  -- | Function to apply on the metadata
  (metaIn -> metaOut) ->
  Circuit (PacketStream dom dataWidth metaIn) (PacketStream dom dataWidth metaOut)
mapMeta f = mapMetaS (pure f)

-- | Sets data bytes that are not enabled in a @PacketStream@ to @0x00@.
zeroOutInvalidBytesC ::
  forall (dom :: Domain) (dataWidth :: Nat) (meta :: Type).
  (KnownNat dataWidth) =>
  (1 <= dataWidth) =>
  Circuit
    (PacketStream dom dataWidth meta)
    (PacketStream dom dataWidth meta)
zeroOutInvalidBytesC = Circuit $ \(fwdIn, bwdIn) -> (bwdIn, fmap (go <$>) fwdIn)
 where
  go transferIn = transferIn{_data = dataOut}
   where
    dataOut = case _last transferIn of
      Nothing -> _data transferIn
      Just i -> a ++ b
       where
        -- The first byte is always valid, so we only map over the rest.
        (a, b') = splitAt d1 (_data transferIn)
        b = imap (\(j :: Index (dataWidth - 1)) byte -> if resize j < i then byte else 0x00) b'
