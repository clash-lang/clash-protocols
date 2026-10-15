{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoFieldSelectors #-}
{-# LANGUAGE NoImplicitPrelude #-}

{- |
Scoreboards and checkers: passive testbench components that check the
transactions a design under test produces. Transactions are lists of values per
clock cycle, as produced by monitors such as
'Protocols.Experimental.Testbench.Df.monitor' combined with 'transactions' or
'transactionsVec'. These components are not tied to a specific protocol.
-}
module Protocols.Experimental.Testbench.Scoreboard (
  -- * Scoreboard
  scoreboard,
  ScoreboardOptions (..),
  defScoreboardOptions,
  Order (..),

  -- * Checker
  checker,

  -- * Transactions
  transactions,
  transactionsVec,
) where

import Clash.Prelude hiding (traceSignal)

import Data.Maybe (isJust, maybeToList)
import Data.Sequence (Seq)
import Data.Text (Text)
import Protocols.Experimental.Testbench.Internal (monus, simMealy)
import Protocols.Experimental.Testbench.Trace
import Protocols.Plugin (CSignal, Circuit (..))

import Data.Foldable qualified as F
import Data.List qualified as L
import Data.Sequence qualified as Seq
import Data.Text qualified as Text

-- | In what order a 'scoreboard' expects outputs
data Order
  = -- | Outputs must appear in the order the model predicted them
    InOrder
  | -- | Outputs may appear in any order
    OutOfOrder
  deriving (Eq, Show, Enum, Bounded)

-- | Options for 'scoreboard'
data ScoreboardOptions = ScoreboardOptions
  { order :: Order
  -- ^ In what order outputs are expected
  , settleCycles :: Natural
  {- ^ Keep the simulation running for this many cycles after the last input or
  output, to catch outputs that should not be there.
  -}
  }
  deriving (Eq, Show)

-- | In order, settling for 16 cycles
defScoreboardOptions :: ScoreboardOptions
defScoreboardOptions = ScoreboardOptions{order = InOrder, settleCycles = 16}

data ScoreboardState s o = ScoreboardState
  { model :: s
  , upfront :: [o]
  -- ^ Expected outputs given up front. Consumed lazily, see 'scoreboard'.
  , pending :: !(Seq o)
  -- ^ Expected outputs predicted by the model
  , settle :: !Natural
  , nOutputs :: !Int
  , failure :: !(Maybe Text)
  }

{- | Check the outputs of a design under test against a reference model. The model
is stepped for every input transaction, and returns the outputs it expects as a
result. Outputs are checked against these expected outputs, either in order or
out of order (see 'Order').

Its status is:

  * 'Error' as soon as an output was not expected;
  * 'PreventStop' while expected outputs are still pending, or while settling
    (see @settleCycles@);
  * 'StopOk' otherwise.

Inputs of a cycle are processed before its outputs, so a combinational design
can produce an output in the same cycle it accepts an input. Outputs given up
front are expected before any outputs predicted by the model. When checking in
order, they are consumed lazily, so a long (or infinite) list of expected outputs
does not need to be kept in memory.

Besides its status, the scoreboard traces the number of outputs predicted by the
model that have not been seen yet (@queued@).
-}
scoreboard ::
  forall dom s i o.
  (KnownDomain dom, Eq o, ShowX o) =>
  -- | Name
  String ->
  Clock dom ->
  ScoreboardOptions ->
  -- | Expected outputs that do not depend on any input
  [o] ->
  -- | Initial state of the model
  s ->
  -- | Model: given an input, returns the outputs it expects
  (s -> i -> (s, [o])) ->
  Circuit (CSignal dom [i], CSignal dom [o]) TraceTree
scoreboard name _clk opts expected0 model0 step = Circuit go
 where
  go (~(ins, outs), _) =
    ( ((), ())
    , traceGroup
        name
        [ traceStatus "status" status
        , traceSignal "queued" nQueued
        ]
    )
   where
    (status, nQueued) = unbundle (simMealy update state0 (bundle (ins, outs)))

  state0 =
    ScoreboardState
      { model = model0
      , upfront = if opts.order == InOrder then expected0 else []
      , pending = if opts.order == InOrder then Seq.empty else Seq.fromList expected0
      , settle = 0
      , nOutputs = 0
      , failure = Nothing
      }

  update ::
    ScoreboardState s o -> ([i], [o]) -> (ScoreboardState s o, (Status, Unsigned 32))
  update st0 ~(newIns, newOuts) = (st3, (scoreboardStatus st3, fromIntegral (Seq.length st3.pending)))
   where
    st1 = L.foldl' feed st0 newIns
    st2 = L.foldl' check st1 newOuts
    active = not (L.null newIns && L.null newOuts)
    st3 = st2{settle = if active then opts.settleCycles else st2.settle `monus` 1}

  feed st i =
    let (m, os) = step st.model i
     in m `seq` st{model = m, pending = st.pending <> Seq.fromList os}

  check st o
    | isJust st.failure = st
    | otherwise =
        case opts.order of
          InOrder ->
            case (st.upfront, st.pending) of
              (e : rest, _) -> compareTo e (\st1 -> st1{upfront = rest})
              ([], e Seq.:<| rest) -> compareTo e (\st1 -> st1{pending = rest})
              ([], Seq.Empty) ->
                fail_ ("unexpected output " <> showT o <> ": no outputs pending")
          OutOfOrder ->
            case Seq.findIndexL (\e -> isX (e == o) == Right True) st.pending of
              Nothing -> fail_ ("unexpected output " <> showT o <> pendingMsg)
              Just ix -> accepted (\st1 -> st1{pending = Seq.deleteAt ix st.pending})
   where
    compareTo e consume =
      case isX (e == o) of
        Right True -> accepted consume
        Right False -> fail_ ("expected " <> showT e <> ", got " <> showT o <> pendingMsg)
        Left _ -> fail_ ("could not compare expected " <> showT e <> " with " <> showT o)
    accepted consume = consume st{nOutputs = st.nOutputs + 1}
    fail_ msg = st{failure = Just ("output #" <> showT' st.nOutputs <> ": " <> msg)}
    pendingMsg = "; pending: " <> showPending (st.upfront <> F.toList st.pending)

  scoreboardStatus st
    | Just msg <- st.failure = Error msg
    | not (L.null st.upfront) || not (Seq.null st.pending) || st.settle > 0 = PreventStop
    | otherwise = StopOk

{- | Check outputs with a stateful predicate, for when outputs cannot be predicted
from inputs. The predicate either returns a new state, or an error.

Its status is 'Error' as soon as the predicate fails, 'PreventStop' while the
state is not done or while settling, and 'StopOk' otherwise.
-}
checker ::
  forall dom s o.
  (KnownDomain dom) =>
  -- | Name
  String ->
  Clock dom ->
  -- | Keep the simulation running for this many cycles after the last output
  Natural ->
  -- | Initial state
  s ->
  -- | Check an output
  (s -> o -> Either Text s) ->
  -- | Whether all expected outputs have been seen
  (s -> Bool) ->
  Circuit (CSignal dom [o]) TraceTree
checker name _clk settleCycles state0 step done = Circuit go
 where
  go (outs, _) = ((), traceGroup name [traceStatus "status" status])
   where
    status = simMealy update (state0, 0, 0, Nothing) outs

  update :: (s, Natural, Int, Maybe Text) -> [o] -> ((s, Natural, Int, Maybe Text), Status)
  update st0 newOuts = (st1, checkerStatus st1)
   where
    (s, settle, n, failure) = L.foldl' check st0 newOuts
    st1 =
      ( s
      , if L.null newOuts then settle `monus` 1 else settleCycles
      , n
      , failure
      )

  check st@(s, settle, n, failure) o
    | isJust failure = st
    | otherwise =
        case step s o of
          Left msg -> (s, settle, n, Just ("output #" <> showT' n <> ": " <> msg))
          Right s1 -> s1 `seq` (s1, settle, n + 1, failure)

  checkerStatus (s, settle, _, failure)
    | Just msg <- failure = Error msg
    | not (done s) || settle > 0 = PreventStop
    | otherwise = StopOk

-- | Turn a stream of optional transactions into a stream of transaction lists
transactions :: Circuit (CSignal dom (Maybe a)) (CSignal dom [a])
transactions = Circuit $ \(txs, _) -> ((), maybeToList <$> txs)

{- | Combine multiple streams of optional transactions, tagging each transaction
with the index of the stream it came from. Transactions of the same cycle are
ordered by index.
-}
transactionsVec ::
  forall n dom a.
  (KnownNat n) =>
  Circuit (Vec n (CSignal dom (Maybe a))) (CSignal dom [(Index n, a)])
transactionsVec = Circuit $ \(txs, _) -> (repeat (), go <$> bundle txs)
 where
  go v = [(i, a) | (i, Just a) <- toList (zip indicesI v)]

showT :: (ShowX a) => a -> Text
showT = Text.pack . showX

showT' :: Int -> Text
showT' = Text.pack . show

showPending :: (ShowX a) => [a] -> Text
showPending s =
  case L.splitAt 5 s of
    (items, []) -> render items
    (items, _) -> render items <> ", .."
 where
  render items = "[" <> Text.intercalate ", " (showT <$> items) <> "]"
