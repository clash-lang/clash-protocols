{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NoFieldSelectors #-}
{-# LANGUAGE NoImplicitPrelude #-}

{- |
Functional coverage. A cover point is a 'Bool' signal that is 'True' in every
cycle something interesting happens, for example a stalled transfer. The runner
counts how often every cover point was hit (the @coverage@ field of
'Protocols.Experimental.Testbench.TestResult').
Using 'Protocols.Experimental.Testbench.Hedgehog.assertTestbench', these counts are
reported to Hedgehog, which adds them up over all runs of a property and checks
them against t'CoverGoal's.

Cover points are named by their path in the 'TraceTree', for example
@fifo\/out\/stall@. Because trace trees are t'SimOnly', a design can also export
cover points of its internals, such as a FIFO being full.
-}
module Protocols.Experimental.Testbench.Coverage (
  cover,
  coverEnum,
  coverTransactions,
  CoverGoal (..),
) where

import Clash.Prelude hiding (traceSignal)

import Data.Text (Text)
import Hedgehog.Internal.Property (CoverPercentage)
import Protocols.Experimental.Testbench.Trace
import Protocols.Plugin (CSignal, Circuit (..))

{- | A goal for a cover point: the minimum percentage of runs of a property in
which the cover point must be hit at least once.
-}
data CoverGoal = CoverGoal
  { path :: Text
  -- ^ Path of the cover point, for example @fifo\/out\/stall@
  , minRuns :: CoverPercentage
  -- ^ Minimum percentage of runs that should hit the cover point
  }
  deriving (Show)

-- | A cover point, hit in every cycle the signal is 'True'
cover :: forall dom. (KnownDomain dom) => String -> Signal dom Bool -> TraceTree
cover name sig =
  SimOnly
    TraceTreeI
      { name = ""
      , traces =
          [ Left
              Trace
                { name
                , period = periodFs @dom
                , samples = CoverSamples (signalToInfinite sig)
                }
          ]
      }

{- | A group of cover points, one for every constructor of @b@, named by 'show'.
Every bin is known up front, so bins that are never hit are reported as such.
-}
coverEnum ::
  forall dom b.
  (KnownDomain dom, Enum b, Bounded b, Show b) =>
  String ->
  Signal dom (Maybe b) ->
  TraceTree
coverEnum name sig =
  traceGroup
    name
    [ cover (show b) ((== Just (fromEnum b)) . fmap fromEnum <$> sig)
    | b <- [minBound .. maxBound :: b]
    ]

{- | Cover the payloads of transactions, for example those produced by
'Protocols.Experimental.Testbench.Df.monitor'. Every transaction is classified
into a bin with the given function, see 'coverEnum'.
-}
coverTransactions ::
  forall dom a b.
  (KnownDomain dom, Enum b, Bounded b, Show b) =>
  String ->
  (a -> b) ->
  Circuit (CSignal dom (Maybe a)) TraceTree
coverTransactions name f = Circuit $ \(txs, _) -> ((), coverEnum name (fmap f <$> txs))
