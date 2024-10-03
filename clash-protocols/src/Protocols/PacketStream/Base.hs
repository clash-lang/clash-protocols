{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_HADDOCK hide #-}

{- |
Definitions and instances of the PacketStream protocol
-}
module Protocols.PacketStream.Base (
  -- * Protocol definition
  PacketStreamM2S (..),
  PacketStreamS2M (..),
  PacketStream,

  -- * CSignal conversion
  toCSignal,
  unsafeFromCSignal,
  unsafeDropBackpressure,

  -- * Basic operations on the PacketStream protocol
  empty,
  consume,
  void,
  fanout,
  forceResetSanity,
  zeroOutInvalidBytesC,
  unsafeAbortOnBackpressureC,

  -- * Skid buffers
  registerBoth,
  registerBwd,
  registerFwd,

  -- * Operations on metadata
  fstMeta,
  sndMeta,
  mapMeta,
  filterMeta,
  firstMeta,
  secondMeta,
  bimapMeta,
  eitherMeta,

  -- * Operations on metadata (Signal versions)
  mapMetaS,
  filterMetaS,
  firstMetaS,
  secondMetaS,
  bimapMetaS,
  eitherMetaS,
) where

import Clash.Prelude hiding (empty, sample)
import qualified Prelude as P

import qualified Data.Bifunctor as B
import Data.Coerce (coerce)
import qualified Data.Maybe as Maybe
import Data.Proxy

import qualified Protocols.Df as Df
import qualified Protocols.DfConv as DfConv
import Protocols.Hedgehog.Internal
import Protocols.Internal

import Control.DeepSeq (NFData)

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
  , _last :: Maybe (Index (dataWidth + 1))
  -- ^ If this is @Just@ then it signals that this transfer is the end of a
  --   packet and contains the number of valid bytes in '_data', starting from
  --   index @0@.
  --
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

deriving instance
  (KnownNat dataWidth, NFDataX meta) =>
  NFDataX (PacketStreamM2S dataWidth meta)

-- | Used by circuit-notation to create an empty stream
instance Default (Maybe (PacketStreamM2S dataWidth meta)) where
  def = Nothing

deriveAutoReg ''PacketStreamM2S

{- |
Data sent from the subordinate to manager.

The only information transmitted is whether the subordinate is ready to receive data.
-}
newtype PacketStreamS2M = PacketStreamS2M
  { _ready :: Bool
  -- ^ Iff True, the subordinate is ready to receive data.
  }
  deriving (Bundle, Eq, Generic, NFData, NFDataX, Show, ShowX)

-- | Used by circuit-notation to create a sink that always acknowledges
instance Default PacketStreamS2M where
  def = PacketStreamS2M True

deriveAutoReg ''PacketStreamS2M

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

{- |
Circuit to convert a 'CSignal' into a 'PacketStream'.
This is unsafe, because it ignores all incoming backpressure.
-}
unsafeFromCSignal ::
  forall dom dataWidth meta.
  Circuit
    (CSignal dom (Maybe (PacketStreamM2S dataWidth meta)))
    (PacketStream dom dataWidth meta)
unsafeFromCSignal = Circuit (\(fwdInS, _) -> (pure (), fwdInS))

-- | Converts a 'PacketStream' into a 'CSignal': always acknowledges.
toCSignal ::
  forall dom dataWidth meta.
  (HiddenClockResetEnable dom) =>
  Circuit
    (PacketStream dom dataWidth meta)
    (CSignal dom (Maybe (PacketStreamM2S dataWidth meta)))
toCSignal = forceResetSanity |> Circuit (\(fwdIn, _) -> (pure (PacketStreamS2M True), fwdIn))

-- | Drop all backpressure signals.
unsafeDropBackpressure ::
  (HiddenClockResetEnable dom) =>
  Circuit
    (PacketStream dom dwIn meta)
    (PacketStream dom dwOut meta) ->
  Circuit
    (CSignal dom (Maybe (PacketStreamM2S dwIn meta)))
    (CSignal dom (Maybe (PacketStreamM2S dwOut meta)))
unsafeDropBackpressure ckt = unsafeFromCSignal |> ckt |> toCSignal

{- |
Sets '_abort' upon receiving backpressure from the subordinate.

__UNSAFE__: because @fwdOut@ depends on @bwdIn@, this may introduce
combinatorial loops.
-}
unsafeAbortOnBackpressureC ::
  forall (dataWidth :: Nat) (meta :: Type) (dom :: Domain).
  (HiddenClockResetEnable dom) =>
  Circuit
    (CSignal dom (Maybe (PacketStreamM2S dataWidth meta)))
    (PacketStream dom dataWidth meta)
unsafeAbortOnBackpressureC = Circuit $ \(fwdInS, bwdInS) -> (pure (), go <$> bundle (fwdInS, bwdInS))
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
      Just i ->
        imap
          (\(j :: Index dataWidth) byte -> if resize j < i then byte else 0x00)
          (_data transferIn)

{- |
Copy data of a single `PacketStream` to multiple. LHS will only receive
an acknowledgement when all RHS receivers have acknowledged data.
-}
fanout ::
  forall n dataWidth meta dom.
  (HiddenClockResetEnable dom) =>
  (KnownNat n) =>
  (KnownNat dataWidth) =>
  (1 <= n) =>
  (NFDataX meta) =>
  Circuit
    (PacketStream dom dataWidth meta)
    (Vec n (PacketStream dom dataWidth meta))
fanout = DfConv.fanout Proxy Proxy

{- |
Place a register on the /forward/ part of a circuit.
This adds combinational delay on the /backward/ path.
-}
registerFwd ::
  forall dataWidth meta dom.
  (HiddenClockResetEnable dom) =>
  (KnownNat dataWidth) =>
  (NFDataX meta) =>
  Circuit (PacketStream dom dataWidth meta) (PacketStream dom dataWidth meta)
registerFwd = DfConv.registerFwd Proxy Proxy

{- |
Place a register on the /backward/ part of a circuit.
This adds combinational delay on the /forward/ path.
-}
registerBwd ::
  forall dataWidth meta dom.
  (HiddenClockResetEnable dom) =>
  (KnownNat dataWidth) =>
  (NFDataX meta) =>
  Circuit (PacketStream dom dataWidth meta) (PacketStream dom dataWidth meta)
registerBwd = DfConv.registerBwd Proxy Proxy

{- |
A pipeline skid buffer: places registers on both the /backward/ and /forward/
part of a circuit. This completely breaks up the combinatorial path between
the left and right side of this component. In order to achieve this, it has to
buffer @Fwd@ twice.

Another benefit of this component is that the circuit on the left hand side
may now use @Bwd@ in order to compute its @Fwd@, because this cannot
introduce combinatorial loops anymore.

Runs at full throughput, but causes 2 clock cycles of latency.
-}
registerBoth ::
  forall dataWidth meta dom.
  (HiddenClockResetEnable dom) =>
  (KnownNat dataWidth) =>
  (NFDataX meta) =>
  Circuit (PacketStream dom dataWidth meta) (PacketStream dom dataWidth meta)
registerBoth = registerBwd |> registerFwd

-- | Never produces a value.
empty :: Circuit () (PacketStream dom dataWidth meta)
empty = Circuit (const ((), pure Nothing))

-- | Always acknowledges incoming data.
consume :: (HiddenReset dom) => Circuit (PacketStream dom dataWidth meta) ()
consume = Circuit (const (pure (PacketStreamS2M True), ()))

-- | Never acknowledges incoming data.
void :: (HiddenClockResetEnable dom) => Circuit (PacketStream dom dataWidth meta) ()
void = DfConv.void Proxy

-- | Like 'P.fst', but over the metadata of a 'PacketStream'.
fstMeta :: Circuit (PacketStream dom dataWidth (a, b)) (PacketStream dom dataWidth a)
fstMeta = mapMeta P.fst

-- | Like 'P.snd', but over the metadata of a 'PacketStream'.
sndMeta :: Circuit (PacketStream dom dataWidth (a, b)) (PacketStream dom dataWidth b)
sndMeta = mapMeta P.snd

-- | Like 'Data.List.map', but over the metadata of a 'PacketStream'.
mapMeta ::
  -- | Function to apply on the metadata
  (metaIn -> metaOut) ->
  Circuit (PacketStream dom dataWidth metaIn) (PacketStream dom dataWidth metaOut)
mapMeta f = mapMetaS (pure f)

-- | Like 'mapMeta', but can reason over signals.
mapMetaS ::
  -- | Function to apply on the metadata, wrapped in a @Signal@
  Signal dom (metaIn -> metaOut) ->
  Circuit (PacketStream dom dataWidth metaIn) (PacketStream dom dataWidth metaOut)
mapMetaS fS = Circuit $ \(fwdIn, bwdIn) -> (bwdIn, go <$> bundle (fwdIn, fS))
 where
  go (inp, f) = (\inPkt -> inPkt{_meta = f (_meta inPkt)}) <$> inp

-- | Like 'Data.List.filter', but over the metadata of a 'PacketStream'.
filterMeta ::
  -- | Predicate which specifies whether to keep a fragment based on its metadata
  (meta -> Bool) ->
  Circuit (PacketStream dom dataWidth meta) (PacketStream dom dataWidth meta)
filterMeta p = filterMetaS (pure p)

-- | Like 'filterMeta', but can reason over signals.
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

-- | Like 'Data.Either.either', but over the metadata of a 'PacketStream'.
eitherMeta ::
  (a -> c) ->
  (b -> c) ->
  Circuit
    (PacketStream dom dataWidth (Either a b))
    (PacketStream dom dataWidth c)
eitherMeta f g = eitherMetaS (pure f) (pure g)

-- | Like 'eitherMeta', but can reason over signals.
eitherMetaS ::
  Signal dom (a -> c) ->
  Signal dom (b -> c) ->
  Circuit
    (PacketStream dom dataWidth (Either a b))
    (PacketStream dom dataWidth c)
eitherMetaS fS gS = mapMetaS (liftA2 P.either fS gS)

-- | Like 'Data.Bifunctor.bimap', but over the metadata of a 'PacketStream'.
bimapMeta ::
  (B.Bifunctor p) =>
  (a -> b) ->
  (c -> d) ->
  Circuit
    (PacketStream dom dataWidth (p a c))
    (PacketStream dom dataWidth (p b d))
bimapMeta f g = bimapMetaS (pure f) (pure g)

-- | Like 'bimapMeta', but can reason over signals.
bimapMetaS ::
  (B.Bifunctor p) =>
  Signal dom (a -> b) ->
  Signal dom (c -> d) ->
  Circuit
    (PacketStream dom dataWidth (p a c))
    (PacketStream dom dataWidth (p b d))
bimapMetaS fS gS = mapMetaS (liftA2 B.bimap fS gS)

-- | Like 'Data.Bifunctor.first', but over the metadata of a 'PacketStream'.
firstMeta ::
  (B.Bifunctor p) =>
  (a -> b) ->
  Circuit
    (PacketStream dom dataWidth (p a c))
    (PacketStream dom dataWidth (p b c))
firstMeta f = firstMetaS (pure f)

-- | Like 'firstMeta', but can reason over signals.
firstMetaS ::
  (B.Bifunctor p) =>
  Signal dom (a -> b) ->
  Circuit
    (PacketStream dom dataWidth (p a c))
    (PacketStream dom dataWidth (p b c))
firstMetaS fS = mapMetaS (B.first <$> fS)

-- | Like 'Data.Bifunctor.second', but over the metadata of a 'PacketStream'.
secondMeta ::
  (B.Bifunctor p) =>
  (b -> c) ->
  Circuit
    (PacketStream dom dataWidth (p a b))
    (PacketStream dom dataWidth (p a c))
secondMeta f = secondMetaS (pure f)

-- | Like 'secondMeta', but can reason over signals.
secondMetaS ::
  (B.Bifunctor p) =>
  Signal dom (b -> c) ->
  Circuit
    (PacketStream dom dataWidth (p a b))
    (PacketStream dom dataWidth (p a c))
secondMetaS fS = mapMetaS (B.second <$> fS)
