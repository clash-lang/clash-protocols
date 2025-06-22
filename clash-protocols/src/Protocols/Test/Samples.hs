{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE NoFieldSelectors #-}

module Protocols.Test.Samples where

import Clash.Explicit.Prelude hiding (bitSize, traceSignal, (:<))

import Clash.Signal.Internal (Clock (..), Signal ((:-)))
import Clash.Sized.Internal.BitVector (BitVector (..))
import Data.Bifunctor (Bifunctor (first, second), bimap)
import Data.List.Infinite (Infinite((:<)))
import Data.List.NonEmpty (NonEmpty((:|)), nonEmpty)
import Data.Either (partitionEithers)
import Data.Maybe (catMaybes)
import Protocols (Df)
import Protocols.Internal (CSignal, Circuit (..), Protocol (..))
import Data.Map (Map)

import Data.List qualified as L
import Data.List.Infinite qualified as I
import Data.List.NonEmpty qualified as NonEmpty
import Data.Map qualified as Map
import Prelude qualified as P

type NonEmptyString = NonEmpty Char

type Picoseconds = Natural
type HectoFemtoseconds = Natural

data Sample = Sample
  { mask :: !Natural
  , value :: !Natural
  }
  deriving (Eq, Show)

data StatusSample
  = -- | Do not shut down simulation. Used if there are still samples to drive.
    PreventStop
  | -- | Error condition. Usually, simulation should stop after this.
    Error String
  | {- | Shutting down simulation is OK. Driver may still be monitoring
    for errors.
    -}
    StopOk
  deriving (Eq, Show)

data Samples
  = SignalSamples Natural (Infinite Sample)
  | StatusSamples (Infinite StatusSample)
  | ClockSamples ActiveEdge

data Trace = Trace
  { name :: String
  -- ^ Name of the trace
  , period :: Picoseconds
  -- ^ Period of the clock
  , samples :: Samples
  }

data TraceTreeI = TraceTreeI
  { name :: String
  -- ^ Name of the trace group. Can be set to an empty string.
  , traces :: [Either Trace TraceTree]
  {- ^ Traces specialized to either \"normal\" samples, or samples from functions
  that indicate whether the simulation should stop or not (and in what manner).
  -}
  }

type TraceTree = SimOnly TraceTreeI

traceClock ::
  forall dom.
  (KnownDomain dom) =>
  String ->
  Clock dom ->
  TraceTree
traceClock name (Clock _ (Just _)) =
  error $ "traceClock: Dynamic clocks are not supported: " <> name
traceClock name (Clock _ Nothing) =
  SimOnly $
    TraceTreeI
      { name = ""
      , traces =
          [ Left $
              Trace
                { name = name
                , period = snatToNum (clockPeriod @dom)
                , samples = ClockSamples $ case activeEdge @dom of
                    SRising -> Rising
                    SFalling -> Falling
                }
          ]
      }

traceReset ::
  forall dom.
  (KnownDomain dom) =>
  String ->
  Reset dom ->
  TraceTree
traceReset name rst = traceSignal (name <> postfix) (unsafeFromReset rst)
 where
  postfix =
    case resetPolarity @dom of
      SActiveHigh -> ""
      SActiveLow -> "_n"

traceSignal ::
  forall dom a.
  (KnownDomain dom, BitPack a) =>
  String ->
  Signal dom a ->
  TraceTree
traceSignal name sig =
  SimOnly $
    TraceTreeI "" $
      [ Left $
          Trace
            { name
            , period = snatToNum (clockPeriod @dom)
            , samples = SignalSamples
                (natToNum @(BitSize a))
                (packToSample <$> signalToInfinite sig)
            }
      ]

traceStatusSignal ::
  forall dom.
  (KnownDomain dom) =>
  String ->
  Signal dom StatusSample ->
  TraceTree
traceStatusSignal name sig =
  SimOnly $
    TraceTreeI "" $
      [ Left $
          Trace
            { name
            , period = snatToNum (clockPeriod @dom)
            , samples = StatusSamples (signalToInfinite sig)
            }
      ]

traceGroup :: String -> [TraceTree] -> TraceTree
traceGroup name traces = SimOnly TraceTreeI{name, traces = Right <$> traces}

instance Protocol TraceTree where
  type Fwd TraceTree = TraceTree
  type Bwd TraceTree = ()

class (Protocol a) => TraceC a where
  traceC :: String -> Circuit a (TraceTree, a)

instance (KnownDomain dom) => TraceC (Clock dom) where
  traceC name =
    Circuit $ \(clk, _) ->
      case clk of
        Clock _ Nothing -> error "traceC: Dynamic clocks are not supported"
        Clock{} -> ((), (traceClock name clk, clk))

instance (KnownDomain dom) => TraceC (Reset dom) where
  traceC name = Circuit $ \(rst, _) -> ((), (traceReset name rst, rst))

instance (KnownDomain dom, BitPack a) => TraceC (CSignal dom a) where
  traceC name = Circuit $ \(fwd, _) -> (pure (), (traceSignal name fwd, fwd))

instance (KnownDomain dom, BitPack a) => TraceC (Df dom a) where
  traceC name = Circuit go
   where
    go (fwd, (_, bwd)) = (bwd, (traceGroup name traces, fwd))
     where
      traces =
        [ traceSignal "data" fwd
        , traceSignal "ack" bwd
        ]

data TestOptions = TestOptions
  { trace :: Bool
  -- ^ Whether to produce a trace file
  , timeout :: Maybe Picoseconds
  -- ^ Timeout in simulation time in picoseconds
  }
  deriving (Eq, Show)

flattenTraceTreeNonEmpty :: TraceTree -> [([NonEmptyString], Trace)]
flattenTraceTreeNonEmpty = L.map (first (catMaybes . L.map nonEmpty)) . flattenTraceTree

flattenTraceTree :: TraceTree -> [([String], Trace)]
flattenTraceTree = L.concatMap (go . Right . SimOnly)
 where
  go :: Either Trace TraceTree -> [([String], Trace)]
  go (Left trace@Trace{name}) = [([name], trace)]
  go (Right (SimOnly TraceTreeI{name, traces})) =
    first (name :) <$> L.concatMap go traces

-- | Convert a bunch of traces (paired with arbitrary IDs) to a priority queue.
toOrderedSamples ::
  forall id.
  NonEmpty (id, Trace) ->
  Infinite (HectoFemtoseconds, ([(id, StatusSample)], [(id, Sample)]))
toOrderedSamples traces = go0 grouped1
 where
  longestPeriod :: HectoFemtoseconds
  longestPeriod = P.maximum (fst <$> grouped0)
  shortestPeriod = P.minimum (fst <$> grouped0)

  grouped1 :: [Infinite (HectoFemtoseconds, ([(id, StatusSample)], [(id, Sample)]))]
  grouped1 = fmap (second partitionEithers) . goAbsolute <$> grouped0
   where
    goAbsolute ::
      (HectoFemtoseconds, [Either (Infinite (id, StatusSample)) (Infinite (id, Sample))]) ->
      Infinite (HectoFemtoseconds, [Either (id, StatusSample) (id, Sample)])
    goAbsolute (period, traces_) = _
     where
      -- traces_ ::

      foo ::
        [Either (Infinite (id, StatusSample)) (Infinite (id, Sample))] ->
        [Infinite (Either (id, StatusSample) (id, Sample))]
      foo = _

      x :: Either (Infinite (id, StatusSample)) [Infinite (id, Sample)]
      x = sequence traces_

  grouped0 :: [(HectoFemtoseconds, [Either (Infinite (id, StatusSample)) (Infinite (id, Sample))])]
  grouped0 = Map.toList $ fromDupList (traceToEither <$> NonEmpty.toList traces)

  traceToEither ::
    (id, Trace) ->
    ( HectoFemtoseconds
    , Either (Infinite (id, StatusSample)) (Infinite (id, Sample))
    )
  traceToEither (id_, Trace{period, samples}) =
    ( period1
    , bimap addId addId samples1
    )
   where
    addId :: Infinite a -> Infinite (id, a)
    addId = fmap (id_,)

    zero = Sample {mask = 0, value = 0}
    one = Sample {mask = 0, value = 1}

    period1 =
      case samples of
        ClockSamples _ -> 5 * period
        StatusSamples _ -> 10 * period
        SignalSamples _ _ -> 10 * period

    samples1 :: Either (Infinite StatusSample) (Infinite Sample)
    samples1 =
      case samples of
        StatusSamples s -> Left s
        SignalSamples _ s -> Right s
        ClockSamples Rising -> Right (I.cycle (zero :| [one]))
        ClockSamples Falling -> Right (I.cycle (one :| [zero]))

  fromDupList :: (Ord k) => [(k, v)] -> Map k [v]
  fromDupList = Map.fromListWith (<>) . fmap (second pure)


--  where
--   -- Find minimum time in next samples
--   go1 ::
--     NonEmpty (id, Infinite (Femtoseconds, sample)) ->
--     Infinite (Femtoseconds, NonEmpty (id, sample))
--   go1 samples = go2 (L.minimum (fst . I.head . snd <$> samples)) samples

--   -- Pop items with given time
--   go2 ::
--     Femtoseconds ->
--     NonEmpty (id, Infinite (Femtoseconds, sample)) ->
--     Infinite (Femtoseconds, NonEmpty (id, sample))
--   go2 now samples = (now, NonEmpty.fromList popped) I.:< go1 rest
--     where
--       (popped, rest) = L.mapAccumL pop [] samples
--       pop acc prev@(id_, I.uncons -> ((timestamp, s), as))
--         | timestamp == now = ((id_, s) : acc, (id_, as))
--         | otherwise = (acc, prev)

-- | Convert a 'Signal' to its (isomorphic) 'Infinite' representation
signalToInfinite :: Signal dom a -> Infinite a
signalToInfinite = go
 where
  go :: Signal dom a -> Infinite a
  go (s :- xs) = s I.:< go xs

-- | Convert a 'BitPack'able thing to a 'Sample'
packToSample :: (BitPack a) => a -> Sample
packToSample (pack -> BV{unsafeMask, unsafeToNatural}) =
  Sample
    { value = unsafeToNatural
    , mask = unsafeMask
    }
