{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NoFieldSelectors #-}
{-# LANGUAGE NoImplicitPrelude #-}

{- |
Traces are the way a testbench communicates with the runner
('Protocols.Experimental.Testbench.runTestbench'). A testbench is a
@t'Circuit' () 'TraceTree'@: a tree of named, sampled signals. Three kinds of
traces steer the simulation:

  * /Status/ traces ('traceStatus') decide when the simulation stops. The runner
    keeps simulating as long as at least one status trace says 'PreventStop', and
    fails the test as soon as one says 'Error'.

  * /Cover/ traces ('Protocols.Experimental.Testbench.Coverage.cover') count how
    often something interesting happened. Their counts end up in the test result.

  * All other traces ('traceSignal', 'traceClock', 'traceReset') are only
    evaluated when the runner writes a waveform (VCD) file.

Because 'TraceTree' is wrapped in t'SimOnly', trace trees are ignored when a
design is turned into HDL. Components can therefore export traces from within a
design.
-}
module Protocols.Experimental.Testbench.Trace (
  -- * Time
  Femtoseconds,
  periodFs,
  cycles,

  -- * Status
  Status (..),

  -- * Trace trees
  TraceTree,
  TraceTreeI (..),
  Trace (..),
  Samples (..),
  Sample (..),
  traceSignal,
  traceClock,
  traceReset,
  traceStatus,
  traceGroup,
  traceGroupC,
  TraceC (..),

  -- * Utilities
  signalToInfinite,
  packToSample,
  isControl,
) where

import Clash.Explicit.Prelude hiding (traceSignal, (:<))
import Prelude qualified as P

import Clash.Signal.Internal (Clock (..), Signal ((:-)))
import Clash.Sized.Internal.BitVector (BitVector (..))
import Control.DeepSeq (rnf)
import Data.Either (isLeft)
import Data.List.Infinite (Infinite ((:<)))
import Data.Text (Text)
import Protocols.Df (Df)
import Protocols.Internal (Ack)
import Protocols.Plugin (CSignal, Circuit (..), Protocol (..))

{- $setup
>>> import Clash.Prelude (System)
-}

{- | Simulation time. Clock periods in Clash are expressed in picoseconds, but
clocks change halfway through their period, so the runner uses femtoseconds.
-}
type Femtoseconds = Natural

-- | Clock period of a domain in femtoseconds.
periodFs :: forall dom. (KnownDomain dom) => Femtoseconds
periodFs = 1000 * snatToNum (clockPeriod @dom)

{- | Duration of a number of clock cycles of a domain. Useful to set a timeout:

>>> cycles @System 10
100000000
-}
cycles :: forall dom. (KnownDomain dom) => Natural -> Femtoseconds
cycles n = n * periodFs @dom

{- | Status of a testbench component, sampled every clock cycle. The runner stops
the simulation as soon as every status trace says 'StopOk', or fails the test
as soon as any status trace says 'Error'.
-}
data Status
  = {- | Do not stop the simulation, for example because a driver still has data
    to send or a checker still expects data.
    -}
    PreventStop
  | {- | Stopping the simulation is fine. The component may still be checking
    for errors.
    -}
    StopOk
  | -- | Something went wrong. The runner fails the test with this message.
    Error Text
  deriving (Eq, Show)

instance NFDataX Status where
  deepErrorX = errorX
  hasUndefined s = case isX s of
    Left _ -> True
    Right (Error msg) -> isLeft (isX msg)
    Right _ -> False
  ensureSpine = id
  rnfX s = case isX s of
    Left _ -> ()
    Right (Error msg) -> either (const ()) rnf (isX msg)
    Right _ -> ()

-- | A single sample of a signal: its bits and a mask marking undefined bits.
data Sample = Sample
  { mask :: !Natural
  -- ^ Bits set in this mask are undefined
  , value :: !Natural
  -- ^ Value of the defined bits
  }
  deriving (Eq, Show)

-- | The samples of a t'Trace'. There is one sample per clock cycle of its domain.
data Samples
  = -- | Samples of a signal of the given bit width
    SignalSamples Int (Infinite Sample)
  | -- | Samples of a status signal, see 'Status'
    StatusSamples (Infinite Status)
  | -- | Samples of a cover point: 'True' in every cycle the cover point was hit
    CoverSamples (Infinite Bool)
  | -- | A clock with the given active edge. Its samples are implied by its period.
    ClockSamples ActiveEdge

-- | A single named trace
data Trace = Trace
  { name :: String
  -- ^ Name of the trace
  , period :: Femtoseconds
  -- ^ Clock period of the domain the trace was sampled in
  , samples :: Samples
  -- ^ Samples, one per clock cycle
  }

-- | A named group of traces. See 'TraceTree'.
data TraceTreeI = TraceTreeI
  { name :: String
  -- ^ Name of this group. Groups with an empty name are flattened into their parent.
  , traces :: [Either Trace TraceTree]
  -- ^ Traces and subgroups
  }

{- | A tree of traces. The tree structure is reflected in the scopes of a waveform
file and in the names of status and cover traces, which are joined with a slash,
for example @fifo\/out\/stall@.
-}
type TraceTree = SimOnly TraceTreeI

instance Protocol TraceTree where
  type Fwd TraceTree = TraceTree
  type Bwd TraceTree = ()

-- | Whether the runner needs to evaluate the trace, even when not writing a waveform
isControl :: Trace -> Bool
isControl trace = case trace.samples of
  StatusSamples _ -> True
  CoverSamples _ -> True
  SignalSamples _ _ -> False
  ClockSamples _ -> False

single :: Trace -> TraceTree
single trace = SimOnly TraceTreeI{name = "", traces = [Left trace]}

-- | Group a number of trace trees under a name
traceGroup :: String -> [TraceTree] -> TraceTree
traceGroup name trees = SimOnly TraceTreeI{name, traces = Right <$> trees}

-- | Circuit version of 'traceGroup'
traceGroupC :: (KnownNat n) => String -> Circuit (Vec n TraceTree) TraceTree
traceGroupC name = Circuit $ \(trees, _) -> (repeat (), traceGroup name (toList trees))

-- | Trace a signal. Undefined bits show up as @x@ in a waveform.
traceSignal ::
  forall dom a.
  (KnownDomain dom, BitPack a) =>
  String ->
  Signal dom a ->
  TraceTree
traceSignal name sig =
  single
    Trace
      { name
      , period = periodFs @dom
      , samples =
          SignalSamples
            (natToNum @(BitSize a))
            (packToSample <$> signalToInfinite sig)
      }

-- | Trace a status signal. See 'Status'.
traceStatus ::
  forall dom.
  (KnownDomain dom) =>
  String ->
  Signal dom Status ->
  TraceTree
traceStatus name sig =
  single
    Trace
      { name
      , period = periodFs @dom
      , samples = StatusSamples (signalToInfinite sig)
      }

{- | Trace a clock. Its active edges line up with the moments other traces of the
same domain change. Dynamic clocks are not supported.
-}
traceClock ::
  forall dom.
  (KnownDomain dom) =>
  String ->
  Clock dom ->
  TraceTree
traceClock name (Clock _ (Just _)) =
  error $ "traceClock: dynamic clocks are not supported: " <> name
traceClock name (Clock _ Nothing) =
  single
    Trace
      { name
      , period = periodFs @dom
      , samples = ClockSamples $ case activeEdge @dom of
          SRising -> Rising
          SFalling -> Falling
      }

-- | Trace a reset. Active-low resets get an @_n@ suffix.
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

-- | Protocols that can be traced with a passthrough circuit
class (Protocol a) => TraceC a where
  -- | Pass a protocol through unchanged, and produce a trace of it
  traceC :: String -> Circuit a (TraceTree, a)

instance (KnownDomain dom) => TraceC (Clock dom) where
  traceC name = Circuit $ \(clk, _) -> ((), (traceClock name clk, clk))

instance (KnownDomain dom) => TraceC (Reset dom) where
  traceC name = Circuit $ \(rst, _) -> ((), (traceReset name rst, rst))

instance (KnownDomain dom, BitPack a) => TraceC (CSignal dom a) where
  traceC name = Circuit $ \(fwd, _) -> ((), (traceSignal name fwd, fwd))

instance (KnownDomain dom, BitPack a) => TraceC (Df dom a) where
  traceC name = Circuit go
   where
    go (fwd, ~(_, bwd)) = (bwd, (traceGroup name traces, fwd))
     where
      traces =
        [ traceSignal "data" fwd
        , traceSignal @dom @Ack "ack" bwd
        ]

-- | Convert a 'Signal' to its (isomorphic) 'Infinite' representation
signalToInfinite :: Signal dom a -> Infinite a
signalToInfinite ~(s :- ss) = s :< signalToInfinite ss

{- | Convert something 'BitPack'able to a t'Sample'. Values that cannot be packed
at all (for example 'errorX') become a sample with all bits undefined.
-}
packToSample :: forall a. (BitPack a) => a -> Sample
packToSample a =
  case isX (pack a) of
    Right (BV{unsafeMask, unsafeToNatural}) ->
      Sample{mask = unsafeMask, value = unsafeToNatural}
    Left _ ->
      Sample{mask = (2 :: Natural) P.^ (natToNum @(BitSize a) :: Int) P.- 1, value = 0}
