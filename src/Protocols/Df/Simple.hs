{-# LANGUAGE NamedFieldPuns #-}
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

import           Prelude

import qualified Data.Bifunctor.Extra as Bifunctor
import           Data.Coerce (coerce)
import           Data.Default (Default)
import           Data.Kind (Type)
import qualified Data.Tuple.Extra as T
import qualified Prelude as P

import           Clash.Prelude (Domain, Signal, type (<=))
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

-- | Data sent over forward channel of 'Dfs'. Note that this data type is strict
-- on its data field. If you need lazy behavior, check out
-- "Protocols.Df.Simple.Lazy".
data Data a
  -- | Send no data
  = NoData
  -- | Send /a/
  | Data !a
  deriving (Functor)

-- | Like 'Protocols.Df.Ack', but carrying phantom type variables to satisfy
-- 'Bwd's injectivity requirement.
newtype Ack a = Ack Bool
  deriving ()

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
