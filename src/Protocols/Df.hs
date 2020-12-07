{-|
Defines data structures and operators to create a Dataflow protocol. For
documentation see:

  * 'Protocols.Circuit'
  * 'Protocols.Df.Df'

This module is designed to be imported using qualified, i.e.:

@
  import qualified Protocols.Df as Df
@
-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NamedFieldPuns #-}

module Protocols.Df
  ( -- * Type definitions
    Df
  , Data(Data, NoData)
  , Ack(..)

    -- * Pure functions
  , map, mapMeta
  , fst, snd
  , mapMaybe, catMaybes
  , filter, filterMeta
  , bimap
  , either
  , first, firstT, mapLeft
  , second, secondT, mapRight
  , const
  , pure

    -- * Stateful functions
  , mealy
  , mealyV
  , fanout
  , registerFwd
  , registerBwd

    -- * Simulation
  , StallAck(..)
  , stall
  , drive
  , sample
  , simulate

    -- * Internals
  , mapInternal
  , resetGen
  ) where

import           Protocols hiding (Ack(..))
import qualified Protocols

import qualified Prelude as P
import           Prelude hiding (map, const, fst, snd, pure, either, filter)

import qualified Data.Bool as B
import           Data.Default
import qualified Data.Bifunctor.Extra as Bifunctor
import qualified Data.Bifunctor as Bifunctor
import           Data.Bifunctor (Bifunctor)
import           Data.Coerce (coerce)
import           Data.Kind (Type)
import           Data.List ((\\))
import qualified Data.Maybe as Maybe
import qualified Data.Tuple.Extra as T
import           GHC.Stack (HasCallStack)
import           GHC.Generics (Generic)

import           Clash.Prelude (Signal, Domain, type (<=), type (-))
import qualified Clash.Prelude as C
import qualified Clash.Explicit.Prelude as CE
import           Clash.Signal.Internal (Signal((:-)))

-- | Base dataflow protocol of 'clash-protocols'. On the forward channel it
-- carries a tuple of metadata (/meta/) and a payload (/a/). The tuple is
-- wrapped in a /Maybe/-like structure, enabling the sender to send no data
-- instead. The backward channel consists of a simple /Ack/ or /Nack/.
--
-- Components implementing this protocol should adhere to the following rules:
--
--   1. The decision to send or not send data should not depend on the
--      acknowledgement signal sent by the receiver.
--
--   2. The data channel should remain stable (i.e., not change) until the
--      receiver has sent an acknowledgement.
--
-- __N.B.__: For performance reasons 'Data' is strict on its fields. That is,
-- if it is evaluated to WHNF, its fields will also be evaluated to WHNF. If you
-- need lazy behavior, check out "Protocols.Df.Lazy".
--
data Df (dom :: Domain) (meta :: Type) (a :: Type)

instance Protocol (Df dom meta a) where
  -- | Forward part of base dataflow: @Signal dom (Data meta a)@
  type Fwd (Df dom meta a) = Signal dom (Data meta a)

  -- | Backward part of base dataflow: @Signal dom (Ack meta a)@
  type Bwd (Df dom meta a) = Signal dom (Ack meta a)

instance ( C.KnownDomain dom
         , C.NFDataX meta, C.ShowX meta, Show meta
         , C.NFDataX a, C.ShowX a, Show a ) => Simulate (Df dom meta a) where
  type SimulateType (Df dom meta a) = [Maybe (meta, a)]
  type SimulateChannels (Df dom meta a) = 1

  driveC SimulationConfig{resetCycles} inp =
    drive (resetGen resetCycles) inp

  sampleC SimulationConfig{resetCycles} =
    sample (resetGen resetCycles) maxBound

  stallC SimulationConfig{resetCycles} (C.head -> (stallAck, stalls)) =
    stall (resetGen resetCycles) stallAck stalls

-- | Data sent over forward channel of 'Df'. Note that this data type is strict
-- on its fields. If you need lazy behavior, check out "Protocols.Df.Lazy".
data Data meta a
  -- | Send no data
  = NoData
  -- | Send /meta/ and /a/
  | Data !meta !a
  deriving (Functor, Generic, C.NFDataX)

instance Bifunctor Data where
  bimap _fab _fcd NoData = NoData
  bimap fab fcd (Data meta dat) = Data (fab meta) (fcd dat)

-- | Like 'Ack', but carrying phantom type variables to satisfy 'Bwd's
-- injectivity requirement.
newtype Ack meta a = Ack Bool
  deriving (Generic, C.NFDataX)

instance Default (Ack meta a) where
  def = Ack True

-- | Convenience function for protocol implementers. Maps over internals of a
-- protocol represented by a single set of signals.
mapInternal ::
  ( Fwd a ~ Signal dom aFwd
  , Bwd b ~ Signal dom bBwd
  , Fwd b ~ Signal dom bFwd
  , Bwd a ~ Signal dom aBwd
  ) =>
  ((aFwd, bBwd) -> (aBwd, bFwd)) ->
  Circuit a b
mapInternal f =
  Circuit (C.unbundle . fmap f . C.bundle)

-- | Like 'C.resetGenN', but works on 'Int' instead of 'C.SNat'. Not
-- synthesizable.
resetGen :: C.KnownDomain dom => Int -> C.Reset dom
resetGen n = C.unsafeFromHighPolarity
  (C.fromList (P.replicate n True <> P.repeat False))

--------------------------------- FUNCTIONS ------------------------------------

-- | Like 'P.map', but over payload (/a/) of a Df stream.
map :: (a -> b) -> Circuit (Df dom meta a) (Df dom meta b)
map f = mapInternal (Bifunctor.swapMap (fmap f) coerce)

-- | Like 'Data.Maybe.catMaybes', but over payload (/a/) of a Df stream.
catMaybes :: Circuit (Df dom meta (Maybe a)) (Df dom meta a)
catMaybes = mapInternal (uncurry go)
 where
  go NoData _ = (Ack False, NoData)
  go (Data _meta Nothing) _ack = (Ack True, NoData)
  go (Data meta (Just dat)) (Ack ack) = (Ack ack, Data meta dat)

-- | Like 'Data.Maybe.mapMaybe', but over payload (/a/) of a Df stream.
mapMaybe :: (a -> Maybe b) -> Circuit (Df dom meta a) (Df dom meta b)
mapMaybe f = map f |> catMaybes

-- | Like 'P.filter', but over payload (/a/) of a Df stream.
filter :: (a -> Bool) -> Circuit (Df dom meta a) (Df dom meta a)
filter f = mapInternal (uncurry go)
 where
  go NoData _ = (Ack False, NoData)
  go (Data meta dat) (Ack ack)
    | f dat = (Ack ack, Data meta dat)
    | otherwise = (Ack True, NoData)

-- | Like 'P.filter', but over metadata (/meta/) of a Df stream.
filterMeta :: (meta -> Bool) -> Circuit (Df dom meta a) (Df dom meta a)
filterMeta f = mapInternal (uncurry go)
 where
  go NoData _ = (Ack False, NoData)
  go (Data meta dat) (Ack ack)
    | f meta = (Ack ack, Data meta dat)
    | otherwise = (Ack True, NoData)

-- | Like 'Bifunctor.bimap', but over payload (/a/) of a Df stream.
bimap ::
  Bifunctor p =>
  (a -> b) ->
  (c -> d) ->
  Circuit
    (Df dom meta (p a c))
    (Df dom meta (p b d))
bimap f g = mapInternal (Bifunctor.swapMap (fmap (Bifunctor.bimap f g)) coerce)

-- | Like 'P.map', but over the metadata (/metaA/) of a Df stream.
mapMeta ::
  (metaA -> metaB) ->
  Circuit (Df dom metaA a) (Df dom metaB a)
mapMeta f = mapInternal (Bifunctor.swapMap (Bifunctor.bimap f P.id) coerce)

-- | Like 'P.fst', but over payload of a Df stream.
fst :: Circuit (Df dom meta (a, b)) (Df dom meta a)
fst = map P.fst

-- | Like 'P.snd', but over payload of a Df stream.
snd :: Circuit (Df dom meta (a, b)) (Df dom meta b)
snd = map P.snd

-- | Like 'Bifunctor.first', but over payload of a Df stream.
first ::
  Bifunctor p =>
  (a -> b) ->
  Circuit
    (Df dom meta (p a c))
    (Df dom meta (p b c))
first f = map (Bifunctor.first f)

-- | Like 'Bifunctor.second', but over payload of a Df stream.
second ::
  Bifunctor p =>
  (b -> c) ->
  Circuit
    (Df dom meta (p a b))
    (Df dom meta (p a c))
second f = map (Bifunctor.second f)

-- | Like 'T.first', but over payload of a Df stream.
firstT :: (x -> y) -> Circuit (Df dom meta (x, z)) (Df dom meta (y, z))
firstT = first

-- | Like 'T.second', but over payload of a Df stream.
secondT :: (y -> z) -> Circuit (Df dom meta (x, y)) (Df dom meta (x, z))
secondT = second

-- | Like 'Data.Either.Combinators.mapLeft', but over payload of a Df stream.
mapLeft ::
  (a -> b) ->
  Circuit
    (Df dom meta (Either a c))
    (Df dom meta (Either b c))
mapLeft = first

-- | Like 'Data.Either.Combinators.mapRight', but over payload of a Df stream.
mapRight ::
  (b -> c) ->
  Circuit
    (Df dom meta (Either a b))
    (Df dom meta (Either a c))
mapRight = second

-- | Like 'Data.Either.either', but over payload of a Df stream.
either ::
  (a -> c) ->
  (b -> c) ->
  Circuit
    (Df dom meta (Either a b))
    (Df dom meta c)
either f g = map (P.either f g)

-- | Acknowledge but ignore data from LHS protocol. Send a static value composed
-- of /meta/ and /payload/.
const ::
  meta ->
  payload ->
  Circuit (Df dom meta_ payload_) (Df dom meta payload)
const meta payload =
  -- TODO: Should this function be strict on its inputs?
  mapInternal (Bifunctor.swapMap (P.const fwd) (P.const bwd))
 where
  fwd = Data meta payload
  bwd = Ack True

-- | Drive a constant value composed of /meta/ and /payload/.
pure ::
  meta ->
  payload ->
  Circuit () (Df dom meta payload)
pure meta payload =
  Circuit (\_ -> ((), P.pure (Data meta payload)))

-- | Mealy machine acting on raw Dfs stream
mealy ::
  ( C.HiddenClockResetEnable dom
  , C.NFDataX s ) =>
  -- | Transition function
  ( s ->
    (Maybe (iMeta, i), Protocols.Ack) ->
    (s, (Protocols.Ack, Maybe (oMeta, o))) ) ->
  -- | Initial state
  s ->
  -- | Circuit analogous to mealy machine
  Circuit (Df dom iMeta i) (Df dom oMeta o)
mealy f initS = forceAckLow |> Circuit (C.mealyB f' initS)
 where
  f' s =
      T.second (Bifunctor.bimap (coerce :: Protocols.Ack -> Ack iMeta i) toDfData)
    . f s
    . Bifunctor.bimap fromDfData (coerce :: Ack oMeta o -> Protocols.Ack)

  toDfData Nothing = NoData
  toDfData (Just (meta, dat))= Data meta dat

  fromDfData NoData = Nothing
  fromDfData (Data meta dat) = Just (meta, dat)

-- | Mealy machine that only transitions on DF transactions
mealyV ::
  forall s i o meta dom .
  ( C.HiddenClockResetEnable dom
  , C.NFDataX s ) =>
  -- | Transition function
  (s -> i -> (s, Maybe o)) ->
  -- | Initial state
  s ->
  -- | Circuit analogous to mealy machine
  Circuit (Df dom meta i) (Df dom meta o)
mealyV f = mealy go
 where
  go s (Nothing, _) = (s, (Protocols.Ack False, Nothing))
  go s0 (Just (meta, dat), ack) =
    case f s0 dat of
      (s1, Nothing) -> (s1, (Protocols.Ack True, Nothing))
      (s1, Just o) ->
        ( B.bool s0 s1 (coerce ack)
        , (coerce ack, Just (meta, o)) )

-- | Copy (meta)data of a single Df stream to multiple. LHS will only receive
-- an acknowledgement when all RHS receivers have acknowledged data.
fanout ::
  forall n dom a meta .
  (C.KnownNat n, C.HiddenClockResetEnable dom, 1 <= n) =>
  Circuit (Df dom meta a) (C.Vec n (Df dom meta a))
fanout = forceAckLow |> goC
 where
  goC =
    Circuit $ \(s2r, r2s) ->
      T.second C.unbundle (C.mealyB f initState (s2r, C.bundle r2s))

  initState :: C.Vec n Bool
  initState = C.repeat False

  f ::
    C.Vec n Bool ->
    (Data meta a, C.Vec n (Ack meta a)) ->
    (C.Vec n Bool, (Ack meta a, C.Vec n (Data meta a)))
  f acked (s2r, acks)
    | NoData <- s2r =
      -- No data on input, send no data, send nack
      (acked, (Ack False, C.repeat NoData))
    | Data meta dat <- s2r =
      -- Data on input
      let
        -- Send data to "clients" that have not acked yet
        valids_ = C.map not acked
        dats = C.map (B.bool NoData (Data meta dat)) valids_

        -- Store new acks, send ack if all "clients" have acked
        acked1 = C.zipWith (||) acked (coerce acks)
        ack = C.fold @_ @(n-1) (&&) acked1
      in
        ( if ack then initState else acked1
        , (coerce ack, dats) )

-- | Force a /nack/ on the backward channel and /no data/ on the forward
-- channel if reset is asserted.
forceAckLow ::
  forall dom meta a .
  C.HiddenClockResetEnable dom =>
  Circuit (Df dom meta a) (Df dom meta a)
forceAckLow =
  Circuit (\(fwd, bwd) -> C.unbundle . fmap f . C.bundle $ (rstLow, fwd, bwd))
 where
  f (True,  _,   _  ) = (Ack False, NoData)
  f (False, fwd, bwd) = (bwd, fwd)
  rstLow = C.unsafeToHighPolarity (C.hasReset @dom)

-- | Place register on /forward/ part of protocol.
registerFwd ::
  forall dom meta a .
  (C.NFDataX meta, C.NFDataX a, C.HiddenClockResetEnable dom) =>
  Circuit (Df dom meta a) (Df dom meta a)
registerFwd = mealy go Nothing
 where
   go ::
    Maybe (meta, a) ->
    (Maybe (meta, a), Protocols.Ack) ->
    (Maybe (meta, a), (Protocols.Ack, Maybe (meta, a)))
   go s0 (iDat, iAck) = (s1, (Protocols.Ack oAck, s0))
    where
     oAck = Maybe.isNothing s0 || coerce iAck
     s1 = if oAck then iDat else s0

-- | Place register on /backward/ part of protocol. This is implemented using a
-- in-logic two-element shift register.
registerBwd ::
  (C.NFDataX meta, C.NFDataX a, C.HiddenClockResetEnable dom) =>
  Circuit (Df dom meta a) (Df dom meta a)
registerBwd = mealy go (Nothing, Nothing)
 where
  go (ra0, rb) (iDat, (iAck :: Protocols.Ack)) =
    (s, (coerce oAck :: Protocols.Ack, rb))
   where
    oAck = Maybe.isNothing ra0
    ra1 = if oAck then iDat else ra0
    s = if Maybe.isNothing rb || coerce iAck then (Nothing, ra1) else (ra1, rb)

--------------------------------- SIMULATE -------------------------------------

-- | Emit values given in list. Emits no data while reset is asserted. Not
-- synthesizable.
drive ::
  forall dom meta a.
  C.KnownDomain dom =>
  CE.Reset dom ->
  [Maybe (meta, a)] ->
  Circuit () (Df dom meta a)
drive rst s0 = Circuit $
    ((),)
  . C.fromList_lazy
  . go s0 (CE.sample (C.unsafeToHighPolarity rst))
  . CE.sample_lazy
  . P.snd
 where
  go _                    []             _           = error "Unexpected end of reset"
  go _                    (True:resets)  ~(ack:acks) = NoData : (ack `C.seqX` go s0 resets acks)
  go []                   (_:resets)     ~(ack:acks) = NoData : (ack `C.seqX` go [] resets acks)
  go (Nothing:is)         (_:resets)     ~(ack:acks) = NoData : (ack `C.seqX` go is resets acks)
  go (Just (meta,dat):is) (_:resets)     ~(ack:acks) =
    Data meta dat : go (if coerce ack then is else Just (meta,dat):is) resets acks

-- | Sample protocol to a list of values. Drops values while reset is asserted.
-- Not synthesizable.
--
-- For a generalized version of 'sample', check out 'sampleC'.
sample ::
  forall dom meta b.
  C.KnownDomain dom =>
  CE.Reset dom ->
  Int ->
  Circuit () (Df dom meta b) ->
  [Maybe (meta, b)]
sample rst timeoutAfter c =
    P.map (\case {Data meta a -> Just (meta, a); NoData -> Nothing})
  $ P.take timeoutAfter
  $ CE.sample_lazy
  $ ignoreWhileInReset
  $ P.snd
  $ toSignals c ((), Ack <$> C.unsafeToLowPolarity rst)
 where
  ignoreWhileInReset s =
    (uncurry (B.bool NoData)) <$>
    C.bundle (s, C.unsafeToLowPolarity rst)

-- | Stall every valid Df packet with a given number of cycles. If there are
-- more valid packets than given numbers, passthrough all valid packets without
-- stalling. Not synthesizable.
--
-- For a generalized version of 'stall', check out 'stallC'.
stall ::
  forall dom meta a.
  ( C.KnownDomain dom
  , HasCallStack ) =>
  CE.Reset dom ->
  -- | Acknowledgement to send when LHS does not send data. Stall will act
  -- transparently when reset is asserted.
  StallAck ->
  -- Number of cycles to stall for every valid Df packet
  [Int] ->
  Circuit (Df dom meta a) (Df dom meta a)
stall rst stallAck stalls = Circuit $
  uncurry (go stallAcks stalls (C.unsafeToHighPolarity rst))
 where
  stallAcks
    | stallAck == StallCycle = [minBound..maxBound] \\ [StallCycle]
    | otherwise = [stallAck]

  toStallAck :: Data meta a -> Ack meta a -> StallAck -> Ack meta a
  toStallAck (Data {}) ack = P.const ack
  toStallAck NoData ack = \case
    StallWithNack -> Ack False
    StallWithAck -> Ack True
    StallWithErrorX -> CE.errorX "No defined ack"
    StallTransparently -> ack
    StallCycle -> Ack False -- shouldn't happen..

  go [] ss rs fwd bwd =
    go stallAcks ss rs fwd bwd

  go (_:sas) _ (True :- rs) (f :- fwd) ~(b :- bwd) =
    Bifunctor.bimap (b :-) (f :-) (go sas stalls rs fwd bwd)

  go (sa:sas) [] (_ :- rs) (f :- fwd) ~(b :- bwd) =
    Bifunctor.bimap (toStallAck f b sa :-) (f :-) (go sas [] rs fwd bwd)

  go (sa:sas) ss (_ :- rs) (NoData :- fwd) ~(b :- bwd) =
    -- Left hand side does not send data, simply replicate that behavior. Right
    -- hand side might send an arbitrary acknowledgement, so we simply pass it
    -- through.
    Bifunctor.bimap (toStallAck NoData b sa :-) (NoData :-) (go sas ss rs fwd bwd)
  go (_sa:sas) (s:ss) (_ :- rs) (f0 :- fwd) ~(b0 :- bwd) =
    let
      -- Stall as long as s > 0. If s ~ 0, we wait for the RHS to acknowledge
      -- the data. As long as RHS does not acknowledge the data, we keep sending
      -- the same data.
      (f1, b1, s1) = case compare 0 s of
        LT -> (NoData, Ack False, pred s:ss)        -- s > 0
        EQ -> (f0, b0, if coerce b0 then ss else s:ss)        -- s ~ 0
        GT -> error ("Unexpected negative stall: " <> show s) -- s < 0
    in
      Bifunctor.bimap (b1 :-) (f1 :-) (go sas s1 rs fwd bwd)

-- | Simulate a single domain protocol. Not synthesizable.
--
-- For a generalized version of 'simulate', check out 'Protocols.simulateC'.
simulate ::
  forall dom meta a b.
  C.KnownDomain dom =>
  -- | Simulation configuration. Use 'def' for sensible defaults.
  SimulationConfig ->
  -- | Circuit to simulate.
  ( C.Clock dom ->
    C.Reset dom ->
    C.Enable dom ->
    Circuit (Df dom meta a) (Df dom meta b) ) ->
  -- | Inputs
  [Maybe (meta, a)] ->
  -- | Outputs
  [Maybe (meta, b)]
simulate SimulationConfig{..} circ inputs =
  sample rst timeoutAfter (drive rst inputs |> circ clk rst ena)
 where
  (clk, rst, ena) = (C.clockGen, resetGen resetCycles, C.enableGen)
