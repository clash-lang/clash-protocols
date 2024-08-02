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
  unsafeToPacketStream,
  fromPacketStream,
  forceResetSanity,
  filterMetaS,
  filterMeta,
  mapMetaS,
  mapMeta,
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

{- | Data sent from manager to subordinate.

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
data PacketStreamM2S (dataWidth :: Nat) (metaType :: Type) = PacketStreamM2S
  { _data :: Vec dataWidth (BitVector 8)
  -- ^ The bytes to be transmitted.
  , _last :: Maybe (Index dataWidth)
  -- ^ If this is @Just@ then it signals that this transfer
  --   is the end of a packet and contains the index of the last valid byte in `_data`.
  --   If it is @Nothing@ then this transfer is not yet the end of a packet and all
  --   bytes are valid. This implies that no null bytes are allowed in the middle of
  --   a packet, only after a packet.
  , _meta :: metaType
  -- ^ Metadata of a packet. Must be constant during a packet.
  , _abort :: Bool
  -- ^ Iff true, the packet corresponding to this transfer is invalid. The subordinate
  --   must either drop the packet or forward the `_abort`.
  }
  deriving (Eq, Generic, ShowX, Show, NFData, Bundle, Functor)

{- | Data sent from the subordinate to manager.

The only information transmitted is whether the subordinate is ready to receive data.
-}
newtype PacketStreamS2M = PacketStreamS2M
  { _ready :: Bool
  -- ^ Iff True, the subordinate is ready to receive data.
  }
  deriving (Eq, Generic, ShowX, Show, NFData, Bundle, NFDataX)

{- | Simple valid-ready streaming protocol for transferring packets between components.

Invariants:

1. A manager must not check the `Bwd` channel when it is sending @Nothing@ over the `Fwd` channel.
2. A manager must keep sending the same data until the subordinate has acknowledged it, i.e. upon observing `_ready` as @True@.
3. A manager must keep the metadata (`_meta`) of an entire packet it sends constant.
4. A subordinate which receives a transfer with `_abort` asserted must either forward this `_abort` or drop the packet.
5. A packet may not be interrupted by another packet.
6. All bytes in `_data` which are not enabled must be 0x00.
-}
data PacketStream (dom :: Domain) (dataWidth :: Nat) (metaType :: Type)

deriving instance
  (KnownNat dataWidth, NFDataX metaType) =>
  NFDataX (PacketStreamM2S dataWidth metaType)

instance Protocol (PacketStream dom dataWidth metaType) where
  type
    Fwd (PacketStream dom dataWidth metaType) =
      Signal dom (Maybe (PacketStreamM2S dataWidth metaType))
  type Bwd (PacketStream dom dataWidth metaType) = Signal dom PacketStreamS2M

instance IdleCircuit (PacketStream dom dataWidth metaType) where
  idleBwd _ = pure (PacketStreamS2M False)
  idleFwd _ = pure Nothing

instance Backpressure (PacketStream dom dataWidth metaType) where
  boolsToBwd _ = fromList_lazy . fmap PacketStreamS2M

instance DfConv.DfConv (PacketStream dom dataWidth metaType) where
  type Dom (PacketStream dom dataWidth metaType) = dom
  type FwdPayload (PacketStream dom dataWidth metaType) = PacketStreamM2S dataWidth metaType

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
  Simulate (PacketStream dom dataWidth metaType)
  where
  type
    SimulateFwdType (PacketStream dom dataWidth metaType) =
      [Maybe (PacketStreamM2S dataWidth metaType)]
  type SimulateBwdType (PacketStream dom dataWidth metaType) = [PacketStreamS2M]
  type SimulateChannels (PacketStream dom dataWidth metaType) = 1

  simToSigFwd _ = fromList_lazy
  simToSigBwd _ = fromList_lazy
  sigToSimFwd _ s = sample_lazy s
  sigToSimBwd _ s = sample_lazy s

  stallC conf (head -> (stallAck, stalls)) =
    withClockResetEnable clockGen resetGen enableGen
      $ DfConv.stall Proxy Proxy conf stallAck stalls

instance
  (KnownDomain dom) =>
  Drivable (PacketStream dom dataWidth metaType)
  where
  type
    ExpectType (PacketStream dom dataWidth metaType) =
      [PacketStreamM2S dataWidth metaType]

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
  , NFDataX metaType
  , NFData metaType
  , ShowX metaType
  , Show metaType
  , Eq metaType
  , KnownDomain dom
  ) =>
  Test (PacketStream dom dataWidth metaType)
  where
  expectN Proxy options sampled =
    expectN (Proxy @(Df.Df dom _)) options
      $ Df.maybeToData
      <$> sampled

-- | Circuit to convert a CSignal into a PacketStream. This is unsafe, because it drops backpressure.
unsafeToPacketStream ::
  Circuit (CSignal dom (Maybe (PacketStreamM2S n a))) (PacketStream dom n a)
unsafeToPacketStream = Circuit (\(fwdInS, _) -> (pure (), fwdInS))

-- | Converts a PacketStream into a CSignal.
fromPacketStream ::
  forall dom n meta.
  (HiddenClockResetEnable dom) =>
  Circuit (PacketStream dom n meta) (CSignal dom (Maybe (PacketStreamM2S n meta)))
fromPacketStream = forceResetSanity |> Circuit (\(inFwd, _) -> (pure (PacketStreamS2M True), inFwd))

{- | Force a /nack/ on the backward channel and /Nothing/ on the forward
channel if reset is asserted.
-}
forceResetSanity ::
  forall dom n meta.
  (HiddenClockResetEnable dom) =>
  Circuit (PacketStream dom n meta) (PacketStream dom n meta)
forceResetSanity = forceResetSanityGeneric

{- | Filter a packet stream based on its metadata,
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

{- | Map a function on the metadata of a packet stream,
  with the function wrapped in a @Signal@.
-}
mapMetaS ::
  -- | Function to apply on the metadata, wrapped in a @Signal@
  Signal dom (a -> b) ->
  Circuit (PacketStream dom dataWidth a) (PacketStream dom dataWidth b)
mapMetaS fS = Circuit $ \(fwdIn, bwdIn) -> (bwdIn, go <$> bundle (fwdIn, fS))
 where
  go (inp, f) = (\inPkt -> inPkt{_meta = f (_meta inPkt)}) <$> inp

-- | Map a function on the metadata of a packet stream.
mapMeta ::
  -- | Function to apply on the metadata
  (a -> b) ->
  Circuit (PacketStream dom dataWidth a) (PacketStream dom dataWidth b)
mapMeta f = mapMetaS (pure f)
