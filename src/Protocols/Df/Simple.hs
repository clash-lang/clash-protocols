{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE FlexibleContexts #-}
{-|
Defines data structures and operators to create a Dataflow protocol that only
carries data, no metadata. For documentation see:

  * 'Protocols.Circuit'
  * 'Protocols.Df.Simple.Dfs'

-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Protocols.Df.Simple where

import           Prelude hiding ((!!), map)

import           Control.Applicative (Alternative((<|>)), Applicative(liftA2))
import qualified Data.Bifunctor.Extra as Bifunctor
import           Data.Bool (bool)
import           Data.Coerce (coerce)
import           Data.Default (Default)
import           Data.Kind (Type)
import qualified Data.Tuple.Extra as T
import qualified Data.List.NonEmpty
import           Data.Maybe (fromMaybe)
import qualified Prelude as P

import           Clash.Prelude (Domain, Signal, type (<=), type (-), (!!))
import qualified Clash.Prelude as C
import qualified Clash.Explicit.Prelude as CE

import           Protocols hiding (Ack(..))
import qualified Protocols
import qualified Protocols.Df as Df
import           Protocols.Df (Df)
import qualified Protocols.DfLike as DfLike
import           Protocols.DfLike (DfLike)

import           GHC.Stack (HasCallStack)

-- | Like 'Protocols.Df.Df', but without metadata.
--
-- __N.B.__: For performance reasons 'Data' is strict on its data field. That
-- is, if 'Data' is evaluated to WHNF, its fields will be evaluated to WHNF
-- too. If you need lazy behavior, check out "Protocols.Df.Simple.Lazy".
data Dfs (dom :: Domain) (a :: Type)

instance Protocol (Dfs dom a) where
  -- | Forward part of simple dataflow: @Signal dom (Data meta a)@
  type Fwd (Dfs dom a) = Signal dom (Data a)

  -- | Backward part of simple dataflow: @Signal dom (Ack meta a)@
  type Bwd (Dfs dom a) = Signal dom (Ack a)

instance Backpressure (Dfs dom a) where
  boolsToBwd = C.fromList_lazy . coerce

-- | Data sent over forward channel of 'Dfs'. Note that this data type is strict
-- on its data field. If you need lazy behavior, check out
-- "Protocols.Df.Simple.Lazy".
data Data a
  -- | Send no data
  = NoData
  -- | Send /a/
  | Data !a
  deriving (Functor)

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

-- | Constructed with 'Data'?
isData :: Data a -> Bool
isData (Data _) = True
isData NoData = False

-- | Constructed with 'NoData'?
isNoData :: Data a -> Bool
isNoData = not . isData

-- | Like 'Protocols.Df.Ack', but carrying phantom type variables to satisfy
-- 'Bwd's injectivity requirement.
newtype Ack a = Ack Bool
  deriving (Show)

instance Default (Ack a) where
  def = Ack True

instance (C.KnownDomain dom, C.NFDataX a, C.ShowX a, Show a) => Simulate (Dfs dom a) where
  type SimulateType (Dfs dom a) = [Maybe a]
  type SimulateChannels (Dfs dom a) = 1

  driveC SimulationConfig{resetCycles} inp =
    drive (Df.resetGen resetCycles) inp

  sampleC SimulationConfig{resetCycles} =
    sample (Df.resetGen resetCycles) maxBound

  stallC SimulationConfig{resetCycles} (C.head -> (stallAck, stalls)) =
    stall (Df.resetGen resetCycles) stallAck stalls

instance DfLike dom (Dfs dom a) where
  type Payload (Dfs dom a)  = a
  type Meta (Dfs dom a) = ()

  fromDf =
    let go = \case {Df.Data () a -> Data a; Df.NoData -> NoData} in
    Df.mapInternal (Bifunctor.swapMap go coerce)

  toDf =
    let go = \case {Data a -> Df.Data () a; NoData -> Df.NoData} in
    Df.mapInternal (Bifunctor.swapMap go coerce)

-- | Interpret simple dataflow carrying a tuple as 'Df' with /meta/ and /payload/
asDf :: Circuit (Dfs dom (meta, payload)) (Df dom meta payload)
asDf = Df.mapInternal (Bifunctor.swapMap go coerce)
 where
  go (Data (meta, a)) = Df.Data meta a
  go NoData = Df.NoData

-- | Interpret 'Df' as simple dataflow carrying a tuple of /meta/ and /payload/
asDfs :: Circuit (Df dom meta payload) (Dfs dom (meta, payload))
asDfs = Df.mapInternal (Bifunctor.swapMap go coerce)
 where
  go (Df.Data meta a) = Data (meta, a)
  go Df.NoData = NoData

-- | Like 'P.map', but over payload (/a/) of a Dfs stream.
map :: (a -> b) -> Circuit (Dfs dom a) (Dfs dom b)
map = DfLike.map

-- | Like 'P.fst', but over payload of a Dfs stream.
fst :: Circuit (Dfs dom (a, b)) (Dfs dom a)
fst = DfLike.fst

-- | Like 'P.snd', but over payload of a Dfs stream.
snd :: Circuit (Dfs dom (a, b)) (Dfs dom b)
snd = DfLike.snd

-- | Like 'Data.Bifunctor.first', but over payload of a Dfs stream.
first :: (a -> b) -> Circuit (Dfs dom (a, c)) (Dfs dom (b, c))
first = DfLike.first

-- | Like 'Data.Bifunctor.second', but over payload of a Dfs stream.
second :: (b -> c) -> Circuit (Dfs dom (a, b)) (Dfs dom (a, c))
second = DfLike.second

-- | Acknowledge but ignore data from LHS protocol. Send a static value /b/.
const :: b -> Circuit (Dfs dom a) (Dfs dom b)
const = DfLike.const ()

-- | Drive a constant value composed of /a/.
pure :: a -> Circuit () (Dfs dom a)
pure = DfLike.pure ()

-- | Like 'Data.Maybe.catMaybes', but over payload (/a/) of a Df stream.
catMaybes :: Circuit (Dfs dom (Maybe a)) (Dfs dom a)
catMaybes = DfLike.catMaybes

-- | Like 'P.filter', but over payload (/a/) of a 'Dfs' stream.
filter :: (a -> Bool) -> Circuit (Dfs dom a) (Dfs dom a)
filter = DfLike.filter

-- | Like 'Data.Either.Combinators.mapLeft', but over payload of a 'Dfs' stream.
mapLeft :: (a -> b) -> Circuit (Dfs dom (Either a c)) (Dfs dom (Either b c))
mapLeft = DfLike.mapLeft

-- | Like 'Data.Either.Combinators.mapRight', but over payload of a 'Dfs' stream.
mapRight :: (b -> c) -> Circuit (Dfs dom (Either a b)) (Dfs dom (Either a c))
mapRight = DfLike.mapRight

-- | Like 'Data.Either.either', but over payload of a 'Dfs' stream.
either :: (a -> c) -> (b -> c) -> Circuit (Dfs dom (Either a b)) (Dfs dom c)
either f g = DfLike.map (P.either f g)

-- | Mealy machine acting on raw Dfs stream
mealy ::
  ( C.HiddenClockResetEnable dom
  , C.NFDataX s ) =>
  -- | Transition function
  ( s ->
    (Maybe i, Protocols.Ack) ->
    (s, (Protocols.Ack, Maybe o)) ) ->
  -- | Initial state
  s ->
  -- | Circuit analogous to mealy machine
  Circuit (Dfs dom i) (Dfs dom o)
mealy f = DfLike.mealy f'
 where
  f' s =
      -- Add "metadata" (empty tuple)
      T.second (T.second (fmap ((),)))
      -- Feed to function not taking metadata
    . f s
      -- Strip "metadata" (empty tuple)
    . T.first (fmap P.snd)

-- | Copy data of a single 'Dfs' stream to multiple. LHS will only receive
-- an acknowledgement when all RHS receivers have acknowledged data.
fanout ::
  forall n dom a .
  (C.KnownNat n, C.HiddenClockResetEnable dom, 1 <= n) =>
  Circuit (Dfs dom a) (C.Vec n (Dfs dom a))
fanout = DfLike.fanout

-- | Merge data of multiple 'Dfs' streams using Monoid's '<>'.
fanin ::
  forall n dom a .
  (C.KnownNat n, C.HiddenClockResetEnable dom, Monoid a, 1 <= n) =>
  Circuit (C.Vec n (Dfs dom a)) (Dfs dom a)
fanin = bundleVec |> map (C.fold @_ @(n-1) (<>))

-- | Bundle a vector of 'Dfs' streams into one.
bundleVec ::
  forall n dom a .
  (C.KnownNat n, C.HiddenClockResetEnable dom, 1 <= n) =>
  Circuit (C.Vec n (Dfs dom a)) (Dfs dom (C.Vec n a))
bundleVec =
  Circuit (T.first C.unbundle . C.unbundle . fmap go . C.bundle . T.first C.bundle)
 where
  go (iDats, ack) = (acks, dat)
   where
    acks = C.repeat (bool (Ack False) (coerce ack) (isData dat))
    dat = sequence iDats

-- | Split up a 'Dfs' stream of a vector into multiple independent 'Dfs' streams.
unbundleVec ::
  forall n dom a .
  (C.KnownNat n, C.NFDataX a, C.HiddenClockResetEnable dom, 1 <= n) =>
  Circuit (Dfs dom (C.Vec n a)) (C.Vec n (Dfs dom a))
unbundleVec =
  Circuit (T.second C.unbundle . C.mealyB go initState . T.second C.bundle)
 where
  initState :: C.Vec n Bool
  initState = C.repeat False

  go ::
    C.Vec n Bool ->
    (Data (C.Vec n a), C.Vec n (Ack a)) ->
    (C.Vec n Bool, (Ack (C.Vec n a), C.Vec n (Data a)))
  go _acked (NoData, _) = (initState, (Ack False, C.repeat NoData))
  go acked (Data dataVec, acks) =
    let
      -- Send data to "clients" that have not acked yet
      valids_ = C.map not acked
      dats = C.zipWith (\d -> bool NoData (Data d)) dataVec valids_

      -- Store new acks, send ack if all "clients" have acked
      acked1 = C.zipWith (||) acked (coerce acks)
      ack = C.fold @_ @(n-1) (&&) acked1
    in
      ( if ack then initState else acked1
      , (coerce ack, dats) )

-- | Distribute data across multiple components on the RHS. Useful if you want
-- to parallelize a workload across multiple (slow) workers. For optimal
-- throughput, you should make sure workers can accept data every /n/ cycles.
roundrobin ::
  forall n dom a .
  (C.KnownNat n, C.HiddenClockResetEnable dom, 1 <= n) =>
  Circuit (Dfs dom a) (C.Vec n (Dfs dom a))
roundrobin =
  Circuit (T.second C.unbundle . C.mealyB go minBound . T.second C.bundle)
 where
  go :: C.Index n -> (Data a, C.Vec n (Ack a)) -> (C.Index n, (Ack a, C.Vec n (Data a)))
  go i (NoData, _) = (i, (Ack False, C.repeat NoData))
  go i0 (datIn, acks) = (i1, (Ack ack, datOut))
   where
    datOut = C.replace i0 datIn (C.repeat NoData)
    i1 = if ack then C.satSucc C.SatWrap i0 else i0
    Ack ack = acks !! i0

-- | Collect mode in 'roundrobinCollect'
data CollectMode
  -- | Collect in a /roundrobin/ fashion. If a component does not produce
  -- data, wait until it does.
  = NoSkip
  -- | Collect in a /roundrobin/ fashion. If a component does not produce
  -- data, skip it and check the next component on the next cycle.
  | Skip
  -- | Check all components in parallel. Biased towards the /last/ Dfs
  -- channel.
  | Parallel

-- | Opposite of 'roundrobin'. Useful to collect data from workers that only
-- produce a result with an interval of /n/ cycles.
roundrobinCollect ::
  forall n dom a .
  (C.KnownNat n, C.HiddenClockResetEnable dom, 1 <= n) =>
  CollectMode ->
  Circuit (C.Vec n (Dfs dom a)) (Dfs dom a)
roundrobinCollect NoSkip =
  Circuit (T.first C.unbundle . C.mealyB go minBound . T.first C.bundle)
 where
  go :: C.Index n -> (C.Vec n (Data a), Ack a) -> (C.Index n, (C.Vec n (Ack a), Data a))
  go i ((!!i) -> dat@(Data _), ~(Ack ack)) =
    ( if ack then C.satSucc C.SatWrap i else i
    , (C.replace i (Ack ack) (C.repeat (Ack False)), dat) )
  go i _ = (i, (C.repeat (Ack False), NoData))

roundrobinCollect Skip =
  Circuit (T.first C.unbundle . C.mealyB go minBound . T.first C.bundle)
 where
  go :: C.Index n -> (C.Vec n (Data a), Ack a) -> (C.Index n, (C.Vec n (Ack a), Data a))
  go i ((!!i) -> dat@(Data _), ~(Ack ack)) =
    ( if ack then C.satSucc C.SatWrap i else i
    , (C.replace i (Ack ack) (C.repeat (Ack False)), dat) )
  go i _ = (C.satSucc C.SatWrap i, (C.repeat (Ack False), NoData))

roundrobinCollect Parallel =
  Circuit (T.first C.unbundle . C.unbundle . fmap go . C.bundle . T.first C.bundle)
 where
  go :: (C.Vec n (Data a), Ack a) -> (C.Vec n (Ack a), Data a)
  go (dats0, ack) = (acks, dat)
   where
    nacks = C.repeat (Ack False)
    acks = fromMaybe nacks ((\i -> C.replace i ack nacks) <$> dataToMaybe iM)
    dats1 = C.zipWith (\i -> fmap (i,)) C.indicesI dats0
    (iM, dat) = Data.List.NonEmpty.unzip (C.fold @_ @(n-1) (<|>) dats1)

-- | Place register on /forward/ part of a circuit.
registerFwd ::
  forall dom a .
  (C.NFDataX a, C.HiddenClockResetEnable dom) =>
  Circuit (Dfs dom a) (Dfs dom a)
registerFwd = DfLike.registerFwd

-- | Place register on /backward/ part of a circuit. This is implemented using a
-- in-logic two-element shift register.
registerBwd ::
  (C.NFDataX a, C.HiddenClockResetEnable dom) =>
  Circuit (Dfs dom a) (Dfs dom a)
registerBwd = DfLike.registerBwd

--------------------------------- SIMULATE -------------------------------------

-- | Emit values given in list. Emits no data while reset is asserted. Not
-- synthesizable.
drive ::
  forall dom a.
  C.KnownDomain dom =>
  CE.Reset dom ->
  [Maybe a] ->
  Circuit () (Dfs dom a)
drive rst s0 =
  Df.drive rst (P.map (fmap ((),)) s0) |> DfLike.fromDf

-- | Sample protocol to a list of values. Drops values while reset is asserted.
-- Not synthesizable.
--
-- For a generalized version of 'sample', check out 'sampleC'.
sample ::
  forall dom b.
  C.KnownDomain dom =>
  CE.Reset dom ->
  Int ->
  Circuit () (Dfs dom b) ->
  [Maybe b]
sample rst timeoutAfter c =
  P.map (fmap P.snd) (Df.sample rst timeoutAfter (c |> DfLike.toDf))

-- | Stall every valid Dfs packet with a given number of cycles. If there are
-- more valid packets than given numbers, passthrough all valid packets without
-- stalling. Not synthesizable.
--
-- For a generalized version of 'stall', check out 'stallC'.
stall ::
  forall dom a.
  ( C.KnownDomain dom
  , HasCallStack ) =>
  CE.Reset dom ->
  -- | Acknowledgement to send when LHS does not send data. Stall will act
  -- transparently when reset is asserted.
  StallAck ->
  -- Number of cycles to stall for every valid Df packet
  [Int] ->
  Circuit (Dfs dom a) (Dfs dom a)
stall rst stallAck stalls =
  DfLike.toDf |> Df.stall rst stallAck stalls |> DfLike.fromDf

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
    Circuit (Dfs dom a) (Dfs dom b) ) ->
  -- | Inputs
  [Maybe a] ->
  -- | Outputs
  [Maybe b]
simulate conf circ inputs =
    P.map (fmap P.snd)
  $ Df.simulate conf circDf
  $ P.map (fmap ((),)) inputs
 where
  circDf clk rst ena = DfLike.fromDf |> circ clk rst ena |> DfLike.toDf
