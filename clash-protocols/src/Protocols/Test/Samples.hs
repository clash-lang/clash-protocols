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
import Data.Either (partitionEithers)
import Data.List.Infinite (Infinite ((:<)))
import Data.List.NonEmpty (NonEmpty ((:|)))
import Data.Map (Map)
import Protocols (Df)
import Protocols.Internal (CSignal, Circuit (..), Protocol (..))

import Data.List qualified as L
import Data.List.Infinite qualified as I
import Data.List.Infinite qualified as Infinite
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

statusSampleToSample :: StatusSample -> Sample
statusSampleToSample ss =
  case isX ss of
    Right PreventStop -> Sample{mask = 0, value = 0}
    Right (Error _) -> Sample{mask = 0, value = 1}
    Right StopOk -> Sample{mask = 0, value = 2}
    Left _ -> Sample{mask = 0b11, value = 0}

samplesWidth :: Samples -> Int
samplesWidth (SignalSamples n _) = n
samplesWidth (StatusSamples _) = 2
samplesWidth (ClockSamples _) = 1

data Samples
  = SignalSamples Int (Infinite Sample)
  | StatusSamples (Infinite StatusSample)
  | ClockSamples ActiveEdge

isStatusSamples :: Samples -> Bool
isStatusSamples (StatusSamples _) = True
isStatusSamples _ = False

data Trace = Trace
  { name :: String
  -- ^ Name of the trace
  , period :: Picoseconds
  -- ^ Period of the clock
  , samples :: Samples
  }

traceWidth :: Trace -> Int
traceWidth Trace{samples} = samplesWidth samples

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
  SimOnly
    $ TraceTreeI
      { name = ""
      , traces =
          [ Left
              $ Trace
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
  SimOnly
    $ TraceTreeI ""
    $ [ Left
          $ Trace
            { name
            , period = snatToNum (clockPeriod @dom)
            , samples =
                SignalSamples
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
  SimOnly
    $ TraceTreeI ""
    $ [ Left
          $ Trace
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
  traceC name = Circuit $ \(fwd, _) -> ((), (traceSignal name fwd, fwd))

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

flattenTraceTree :: TraceTree -> Maybe (NonEmpty ([String], Trace))
flattenTraceTree traceTree =
  case L.concatMap (go . Right . SimOnly) traceTree of
    [] -> Nothing
    (x : xs) -> Just (x :| xs)
 where
  go :: Either Trace TraceTree -> [([String], Trace)]
  go (Left trace@Trace{name}) = [([name], trace)]
  go (Right (SimOnly TraceTreeI{name, traces})) =
    first (name :) <$> L.concatMap go traces

fromDupList :: (Ord k, Semigroup (f v), Applicative f) => [(k, v)] -> Map k (f v)
fromDupList = Map.fromListWith (<>) . fmap (second pure)

-- | Convert a bunch of traces (paired with arbitrary IDs) to a priority queue.
toOrderedSamples ::
  forall id.
  NonEmpty (id, Trace) ->
  Infinite (HectoFemtoseconds, ([(id, StatusSample)], [(id, Sample)]))
toOrderedSamples traces = nexts timestamped
 where
  nexts ::
    NonEmpty (Infinite (HectoFemtoseconds, ([(id, StatusSample)], [(id, Sample)]))) ->
    Infinite (HectoFemtoseconds, ([(id, StatusSample)], [(id, Sample)]))
  nexts s0 = (nextTimestamp, mconcat currentSamples) :< nexts s1
   where
    (currentSamples, s1) = L.mapAccumL goNext [] s0
    goNext accum (trace@((ts, samples) :< traceRest))
      | ts == nextTimestamp = (samples : accum, traceRest)
      | otherwise = (accum, trace)

    nextTimestamp :: HectoFemtoseconds
    nextTimestamp = P.minimum (fst . fst . Infinite.uncons <$> s0)

  -- Like 'byPeriod', but with (absolute) timestamps coupled to each (status)
  -- sample.
  timestamped ::
    NonEmpty (Infinite (HectoFemtoseconds, ([(id, StatusSample)], [(id, Sample)])))
  timestamped = go0 <$> byPeriod
   where
    go0 (period, traces0) = go1 timestamps (partitionEithers traces0)
     where
      timestamps = 0 :< Infinite.iterate (+ period) longestPeriod
      go1 (t :< ts) (ls0, rs0) = (t, (l, r)) :< go1 ts (ls1, rs1)
       where
        (l, ls1) = L.unzip (Infinite.uncons <$> ls0)
        (r, rs1) = L.unzip (Infinite.uncons <$> rs0)

  -- Given traces grouped by period. Each period has two groups: a bunch of
  -- streams controlling the simulation (e.g. status signals), and a bunch of
  -- streams with actual samples meant for waveform production.
  byPeriod ::
    NonEmpty
      (HectoFemtoseconds, [Either (Infinite (id, StatusSample)) (Infinite (id, Sample))])
  byPeriod =
    NonEmpty.fromList
      $ Map.toList
      $ fromDupList
      $ fmap traceToEither
      $ NonEmpty.toList traces

  -- Convert a trace to a pair of period and samples. Samples are either
  -- status samples (which indicate whether the simulation should stop or not),
  -- or regular samples (which are used for waveform production). Note that
  -- internally, this function will convert any clocks to a "normal" 'Sample'
  -- flipping between 0 and 1.
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

    zero = Sample{mask = 0, value = 0}
    one = Sample{mask = 0, value = 1}

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

  longestPeriod :: HectoFemtoseconds
  longestPeriod = P.maximum (fst <$> byPeriod)

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
