{-|
Defines data structures and operators to create a Dataflow protocol that only
carries data, no metadata. For documentation see:

  * 'Protocols.Circuit'
  * 'Protocols.Df.Df'

-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module Protocols.Df
  ( -- * Types
    Df, Data(..)

    -- * Operations on Df like protocols
  , const, void, pure
  , map, bimap
  , fst, snd
  , mapMaybe, catMaybes
  , filter
  , either
  , first, {-firstT,-} mapLeft
  , second, {-secondT,-} mapRight
  , zipWith, zip
  , partition
  , route
  , select
  , selectN
  , selectUntil
  , fanin
  , mfanin
  , fanout
  , bundleVec
  , unbundleVec
  , roundrobin
  , DfLike.CollectMode(..)
  , roundrobinCollect
  , registerFwd
  , registerBwd

    -- * Simulation functions
  , drive
  , stall
  , sample
  , simulate

    -- * Internals
  , forceResetSanity
  ) where

-- base
import           Control.Applicative (Alternative((<|>)), Applicative(liftA2))
import           Control.DeepSeq (NFData)
import           GHC.Generics (Generic)
import           GHC.Stack (HasCallStack)
import           Prelude hiding
  ((!!), map, zip, zipWith, filter, fst, snd, either, const, pure)


import           Data.Bifunctor (Bifunctor)
import           Data.Coerce (coerce)
import           Data.Kind (Type)
import qualified Data.Maybe as Maybe
import           Data.Proxy
import qualified Prelude as P

-- clash-prelude
import           Clash.Prelude (type (<=))
import           Clash.Signal.Internal (Signal)
import qualified Clash.Prelude as C

-- me
import           Protocols.Internal
import           Protocols.DfLike (DfLike)
import qualified Protocols.DfLike as DfLike

-- $setup
-- >>> import Protocols
-- >>> import Clash.Prelude (Vec(..))
-- >>> import qualified Prelude as P
-- >>> import qualified Data.Bifunctor as B

-- | Like 'Protocols.Df', but without metadata.
--
-- __N.B.__: For performance reasons 'Protocols.Df.Data' is strict on
-- its data field. That is, if 'Protocols.Df.Data' is evaluated to WHNF,
-- its fields will be evaluated to WHNF too.
data Df (dom :: C.Domain) (a :: Type)

instance Protocol (Df dom a) where
  -- | Forward part of simple dataflow: @Signal dom (Data meta a)@
  type Fwd (Df dom a) = Signal dom (Data a)

  -- | Backward part of simple dataflow: @Signal dom Bool@
  type Bwd (Df dom a) = Signal dom Ack

instance Backpressure (Df dom a) where
  boolsToBwd _ = C.fromList_lazy . coerce

-- | Data sent over forward channel of 'Df'. Note that this data type is strict
-- on its data field.
data Data a
  -- | Send no data
  = NoData
  -- | Send /a/
  | Data !a
  deriving (Functor, Generic, C.NFDataX, C.ShowX, Eq, NFData, Show)

instance Applicative Data where
  pure = Data

  liftA2 f (Data a) (Data b) = Data (f a b)
  liftA2 _ _        _        = NoData

instance Alternative Data where
  empty = NoData

  Data a <|> _ = Data a
  _      <|> b = b

instance Monad Data where
  (>>=) :: Data a -> (a -> Data b) -> Data b
  NoData >>= _f = NoData
  Data a >>=  f = f a

-- | Convert 'Data' to 'Maybe'. Produces 'Just' on 'Data', 'Nothing' on 'NoData'.
dataToMaybe :: Data a -> Maybe a
dataToMaybe NoData = Nothing
dataToMaybe (Data a) = Just a

instance (C.KnownDomain dom, C.NFDataX a, C.ShowX a, Show a) => Simulate (Df dom a) where
  type SimulateType (Df dom a) = [Data a]
  type ExpectType (Df dom a) = [a]
  type SimulateChannels (Df dom a) = 1

  toSimulateType Proxy = P.map Data
  fromSimulateType Proxy = Maybe.mapMaybe dataToMaybe

  driveC = drive
  sampleC = sample
  stallC conf (C.head -> (stallAck, stalls)) = stall conf stallAck stalls

instance DfLike dom (Df dom) a where
  type Data (Df dom) a = Data a
  type Payload a = a
  type Ack (Df dom) a = Ack

  getPayload _ (Data a) = Just a
  getPayload _ NoData = Nothing
  {-# INLINE getPayload #-}

  setPayload _ _ _ (Just b) = Data b
  setPayload _ _ _ Nothing = NoData
  {-# INLINE setPayload #-}

  noData _ = NoData
  {-# INLINE noData #-}

  boolToAck _ = coerce
  {-# INLINE boolToAck #-}

  ackToBool _ = coerce
  {-# INLINE ackToBool #-}

-- | Interpret simple dataflow carrying a tuple as 'Df' with /meta/ and /payload/
-- asDf :: Circuit (Df dom (meta, payload)) (Df dom meta payload)
-- asDf = Df.mapInternal (B.swapMap go coerce)
--  where
--   go (Data (meta, a)) = Df.Data meta a
--   go NoData = Df.NoData

-- -- | Interpret 'Df' as simple dataflow carrying a tuple of /meta/ and /payload/
-- asDf :: Circuit (Df dom meta payload) (Df dom (meta, payload))
-- asDf = Df.mapInternal (B.swapMap go coerce)
--  where
--   go (Df.Data meta a) = Data (meta, a)
--   go Df.NoData = NoData

-- | Force a /nack/ on the backward channel and /no data/ on the forward
-- channel if reset is asserted.
forceResetSanity :: forall dom a. C.HiddenClockResetEnable dom => Circuit (Df dom a) (Df dom a)
forceResetSanity = DfLike.forceResetSanity Proxy

-- | Like 'P.map', but over payload (/a/) of a Df stream.
map :: (a -> b) -> Circuit (Df dom a) (Df dom b)
map = DfLike.map Proxy Proxy

-- | Like 'P.map', but over payload (/a/) of a Df stream.
bimap :: Bifunctor p => (a -> b) -> (c -> d) -> Circuit (Df dom (p a c)) (Df dom (p b d))
bimap = DfLike.bimap

-- | Like 'P.fst', but over payload of a Df stream.
fst :: Circuit (Df dom (a, b)) (Df dom a)
fst = DfLike.fst

-- | Like 'P.snd', but over payload of a Df stream.
snd :: Circuit (Df dom (a, b)) (Df dom b)
snd = DfLike.snd

-- | Like 'Data.Bifunctor.first', but over payload of a Df stream.
first :: Bifunctor p => (a -> b) -> Circuit (Df dom (p a c)) (Df dom (p b c))
first = DfLike.first

-- | Like 'Data.Bifunctor.second', but over payload of a Df stream.
second :: Bifunctor p => (b -> c) -> Circuit (Df dom (p a b)) (Df dom (p a c))
second = DfLike.second

-- | Acknowledge but ignore data from LHS protocol. Send a static value /b/.
const :: C.HiddenReset dom => Data b -> Circuit (Df dom a) (Df dom b)
const = DfLike.const Proxy Proxy

-- | Drive a constant value composed of /a/.
pure :: Data a -> Circuit () (Df dom a)
pure = DfLike.pure Proxy

-- | Drive a constant value composed of /a/.
void :: C.HiddenReset dom => Circuit (Df dom a) ()
void = DfLike.void Proxy

-- | Like 'Data.Maybe.catMaybes', but over a Df stream.
--
-- Example:
--
-- >>> take 2 (simulateCS (catMaybes @C.System @Int) [Nothing, Just 1, Nothing, Just 3])
-- [1,3]
--
catMaybes :: Circuit (Df dom (Maybe a)) (Df dom a)
catMaybes = DfLike.catMaybes Proxy Proxy

-- | Like 'Data.Maybe.mapMaybe', but over payload (/a/) of a Df stream.
mapMaybe :: (a -> Maybe b) -> Circuit (Df dom a) (Df dom b)
mapMaybe = DfLike.mapMaybe

-- | Like 'P.filter', but over a 'Df' stream.
--
-- Example:
--
-- >>> take 3 (simulateCS (filter @C.System @Int (>5)) [1, 5, 7, 10, 3, 11])
-- [7,10,11]
--
filter :: forall dom a. (a -> Bool) -> Circuit (Df dom a) (Df dom a)
filter = DfLike.filter Proxy

-- | Like 'Data.Either.Combinators.mapLeft', but over payload of a 'Df' stream.
mapLeft :: (a -> b) -> Circuit (Df dom (Either a c)) (Df dom (Either b c))
mapLeft = DfLike.mapLeft

-- | Like 'Data.Either.Combinators.mapRight', but over payload of a 'Df' stream.
mapRight :: (b -> c) -> Circuit (Df dom (Either a b)) (Df dom (Either a c))
mapRight = DfLike.mapRight

-- | Like 'Data.Either.either', but over a 'Df' stream.
either :: (a -> c) -> (b -> c) -> Circuit (Df dom (Either a b)) (Df dom c)
either = DfLike.either

-- | Like 'P.zipWith', but over two 'Df' streams.
--
-- Example:
--
-- >>> take 3 (simulateCS (zipWith @C.System @Int (+)) ([1, 3, 5], [2, 4, 7]))
-- [3,7,12]
--
zipWith ::
  forall dom a b c.
  (a -> b -> c) ->
  Circuit
    (Df dom a, Df dom b)
    (Df dom c)
zipWith = DfLike.zipWith Proxy Proxy Proxy

-- | Like 'P.zip', but over two 'Df' streams.
zip :: forall a b dom. Circuit (Df dom a, Df dom b) (Df dom (a, b))
zip = DfLike.zip Proxy Proxy Proxy

-- | Like 'P.partition', but over 'Df' streams
--
-- Example:
--
-- >>> let input = [1, 3, 5, 7, 9, 2, 11]
-- >>> let output = simulateCS (partition @C.System @Int (>5)) input
-- >>> B.bimap (take 3) (take 4) output
-- ([7,9,11],[1,3,5,2])
--
partition :: forall dom a. (a -> Bool) -> Circuit (Df dom a) (Df dom a, Df dom a)
partition = DfLike.partition Proxy

-- | Route a 'Df' stream to another corresponding to the index
--
-- Example:
--
-- >>> let input = [(0, 3), (0, 5), (1, 7), (2, 13), (1, 11), (2, 1)]
-- >>> let output = simulateCS (route @3 @C.System @Int) input
-- >>> fmap (take 2) output
-- <[3,5],[7,11],[13,1]>
--
route ::
  forall n dom a. C.KnownNat n =>
  Circuit (Df dom (C.Index n, a)) (C.Vec n (Df dom a))
route = DfLike.route Proxy Proxy

-- | Select data from the channel indicated by the 'Df' stream carrying
-- @Index n@.
--
-- Example:
--
-- >>> let indices = [1, 1, 2, 0, 2]
-- >>> let dats = [8] :> [5, 7] :> [9, 1] :> Nil
-- >>> let output = simulateCS (select @3 @C.System @Int) (dats, indices)
-- >>> take 5 output
-- [5,7,9,8,1]
--
select ::
  forall n dom a.
  C.KnownNat n =>
  Circuit (C.Vec n (Df dom a), Df dom (C.Index n)) (Df dom a)
select = DfLike.select

-- | Select /selectN/ samples from channel /n/.
--
-- Example:
--
-- >>> let indices = [(0, 2), (1, 3), (0, 2)]
-- >>> let dats = [10, 20, 30, 40] :> [11, 22, 33] :> Nil
-- >>> let circuit = C.exposeClockResetEnable (selectN @2 @10 @C.System @Int)
-- >>> take 7 (simulateCSE circuit (dats, indices))
-- [10,20,11,22,33,30,40]
--
selectN ::
  forall n selectN dom a.
  ( C.HiddenClockResetEnable dom
  , C.KnownNat selectN
  , C.KnownNat n
  ) =>
  Circuit
    (C.Vec n (Df dom a), Df dom (C.Index n, C.Index selectN))
    (Df dom a)
selectN = DfLike.selectN Proxy Proxy

-- | Selects samples from channel /n/ until the predicate holds. The cycle in
-- which the predicate turns true is included.
--
-- Example:
--
-- >>> let indices = [0, 0, 1, 2]
-- >>> let channel1 = [(10, False), (20, False), (30, True), (40, True)]
-- >>> let channel2 = [(11, False), (21, True)]
-- >>> let channel3 = [(12, False), (22, False), (32, False), (42, True)]
-- >>> let dats = channel1 :> channel2 :> channel3 :> Nil
-- >>> take 10 (simulateCS (selectUntil @3 @C.System @(Int, Bool) P.snd) (dats, indices))
-- [(10,False),(20,False),(30,True),(40,True),(11,False),(21,True),(12,False),(22,False),(32,False),(42,True)]
--
selectUntil ::
  forall n dom a.
  C.KnownNat n =>
  (a -> Bool) ->
  Circuit
    (C.Vec n (Df dom a), Df dom (C.Index n))
    (Df dom a)
selectUntil = DfLike.selectUntil Proxy Proxy

-- | Copy data of a single 'Df' stream to multiple. LHS will only receive
-- an acknowledgement when all RHS receivers have acknowledged data.
fanout ::
  forall n dom a .
  (C.KnownNat n, C.HiddenClockResetEnable dom, 1 <= n) =>
  Circuit (Df dom a) (C.Vec n (Df dom a))
fanout = DfLike.fanout Proxy

-- | Merge data of multiple 'Df' streams using a user supplied function
fanin ::
  forall n dom a .
  (C.KnownNat n, 1 <= n) =>
  (a -> a -> a) ->
  Circuit (C.Vec n (Df dom a)) (Df dom a)
fanin = DfLike.fanin

-- | Merge data of multiple 'Df' streams using Monoid's '<>'.
mfanin ::
  forall n dom a .
  (C.KnownNat n, Monoid a, 1 <= n) =>
  Circuit (C.Vec n (Df dom a)) (Df dom a)
mfanin = DfLike.mfanin

-- | Bundle a vector of 'Df' streams into one.
bundleVec ::
  forall n dom a .
  (C.KnownNat n, 1 <= n) =>
  Circuit (C.Vec n (Df dom a)) (Df dom (C.Vec n a))
bundleVec = DfLike.bundleVec Proxy Proxy

-- | Split up a 'Df' stream of a vector into multiple independent 'Df' streams.
unbundleVec ::
  forall n dom a .
  (C.KnownNat n, C.NFDataX a, C.HiddenClockResetEnable dom, 1 <= n) =>
  Circuit (Df dom (C.Vec n a)) (C.Vec n (Df dom a))
unbundleVec = DfLike.unbundleVec Proxy Proxy

-- | Distribute data across multiple components on the RHS. Useful if you want
-- to parallelize a workload across multiple (slow) workers. For optimal
-- throughput, you should make sure workers can accept data every /n/ cycles.
roundrobin ::
  forall n dom a .
  (C.KnownNat n, C.HiddenClockResetEnable dom, 1 <= n) =>
  Circuit (Df dom a) (C.Vec n (Df dom a))
roundrobin = DfLike.roundrobin Proxy

-- | Opposite of 'roundrobin'. Useful to collect data from workers that only
-- produce a result with an interval of /n/ cycles.
roundrobinCollect ::
  forall n dom a .
  (C.KnownNat n, C.HiddenClockResetEnable dom, 1 <= n) =>
  DfLike.CollectMode ->
  Circuit (C.Vec n (Df dom a)) (Df dom a)
roundrobinCollect = DfLike.roundrobinCollect Proxy

-- | Place register on /forward/ part of a circuit.
registerFwd ::
  forall dom a .
  (C.NFDataX a, C.HiddenClockResetEnable dom) =>
  Circuit (Df dom a) (Df dom a)
registerFwd = DfLike.registerFwd Proxy

-- | Place register on /backward/ part of a circuit. This is implemented using a
-- in-logic two-element shift register.
registerBwd ::
  (C.NFDataX a, C.HiddenClockResetEnable dom) =>
  Circuit (Df dom a) (Df dom a)
registerBwd = DfLike.registerBwd Proxy

--------------------------------- SIMULATE -------------------------------------

-- | Emit values given in list. Emits no data while reset is asserted. Not
-- synthesizable.
drive ::
  forall dom a.
  C.KnownDomain dom =>
  SimulationConfig ->
  [Data a] ->
  Circuit () (Df dom a)
drive = DfLike.drive Proxy

-- | Sample protocol to a list of values. Drops values while reset is asserted.
-- Not synthesizable.
--
-- For a generalized version of 'sample', check out 'sampleC'.
sample ::
  forall dom b.
  C.KnownDomain dom =>
  SimulationConfig ->
  Circuit () (Df dom b) ->
  [Data b]
sample = DfLike.sample Proxy

-- | Stall every valid Df packet with a given number of cycles. If there are
-- more valid packets than given numbers, passthrough all valid packets without
-- stalling. Not synthesizable.
--
-- For a generalized version of 'stall', check out 'stallC'.
stall ::
  forall dom a.
  ( C.KnownDomain dom
  , HasCallStack ) =>
  SimulationConfig ->
  -- | Acknowledgement to send when LHS does not send data. Stall will act
  -- transparently when reset is asserted.
  StallAck ->
  -- Number of cycles to stall for every valid Df packet
  [Int] ->
  Circuit (Df dom a) (Df dom a)
stall = DfLike.stall Proxy

-- | Simulate a single domain protocol. Not synthesizable.
--
-- For a generalized version of 'simulate', check out 'Protocols.simulateC'.
simulate ::
  forall dom a b.
  C.KnownDomain dom =>
  -- | Simulation configuration. Use 'Data.Default.def' for sensible defaults.
  SimulationConfig ->
  -- | Circuit to simulate.
  ( C.Clock dom ->
    C.Reset dom ->
    C.Enable dom ->
    Circuit (Df dom a) (Df dom b) ) ->
  -- | Inputs
  [Data a] ->
  -- | Outputs
  [Data b]
simulate = DfLike.simulate Proxy Proxy
