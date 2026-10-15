{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoFieldSelectors #-}
{-# LANGUAGE NoImplicitPrelude #-}

{- |
Testbench components for 'Df':

  * A 'driver' sends a list of values into a design under test.
  * A 'sink' consumes values, applying backpressure.
  * A 'monitor' passively checks the 'Df' invariants on a channel, traces it,
    records coverage, and outputs its transactions.
  * 'expect', 'expectUnordered', and 'expectWith' combine a sink, a monitor, and
    a checker to check the output of a design under test.

All components trace the channel they are connected to, check the 'Df'
invariants, and record the following cover points:

  [@transfer@]: data was transferred
  [@stall@]: data was offered but not acknowledged
  [@idle@]: no data was offered
  [@back_to_back@]: data was transferred in two consecutive cycles
  [@ack_while_idle@]: an acknowledgement was sent while no data was offered
  [@long_stall@]: data was offered for four or more cycles without being acknowledged

All of these are only recorded outside of reset.
-}
module Protocols.Experimental.Testbench.Df (
  -- * Driver
  driver,
  DriverOptions (..),
  defDriverOptions,

  -- * Sink
  sink,
  SinkOptions (..),
  defSinkOptions,
  IdleAck (..),

  -- * Monitor
  monitor,

  -- * Expecting data
  expect,
  expectUnordered,
  expectWith,
  ExpectOptions (..),
  defExpectOptions,

  -- * Generators
  genStalls,
  genDriverOptions,
  genSinkOptions,
  genExpectOptions,
) where

import Clash.Prelude hiding (traceSignal)

import Data.Maybe (fromMaybe, listToMaybe, maybeToList)
import Data.Text (Text)
import Hedgehog (Gen)
import Protocols.Df (Df)
import Protocols.Experimental.Testbench.Coverage (cover)
import Protocols.Experimental.Testbench.Internal (showText, simMealy)
import Protocols.Experimental.Testbench.Scoreboard
import Protocols.Experimental.Testbench.Trace
import Protocols.Internal (Ack (..))
import Protocols.Plugin (CSignal, Circuit (..))

import Data.List qualified as L
import Data.Text qualified as Text
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range

-- | Options for 'driver'
data DriverOptions = DriverOptions
  { idleCycles :: [Natural]
  {- ^ Number of cycles to wait before offering each value. Once the list runs
  out, values are offered without waiting.
  -}
  , driveDuringReset :: Bool
  {- ^ Offer the first value while reset is asserted. The design under test may
  not acknowledge it until reset is deasserted.
  -}
  }
  deriving (Eq, Show)

-- | No idle cycles, don't drive during reset
defDriverOptions :: DriverOptions
defDriverOptions = DriverOptions{idleCycles = [], driveDuringReset = False}

data DriverState a = DriverState
  { items :: [a]
  , idles :: [Natural]
  , driverWaiting :: !(Maybe Natural)
  , nSent :: !Int
  , driverFailure :: !(Maybe Text)
  }

{- | Send a list of values. Every value is offered until it is acknowledged, see the
options in t'DriverOptions'. Its status is 'PreventStop' until every value has been
acknowledged. It reports an 'Error' if the design under test acknowledges data
during reset, or sends an undefined acknowledgement while data is offered.
-}
driver ::
  forall dom a.
  (KnownDomain dom, BitPack a, ShowX a) =>
  -- | Name
  String ->
  Clock dom ->
  Reset dom ->
  DriverOptions ->
  -- | Values to send
  [a] ->
  Circuit () (TraceTree, Df dom a)
driver name _clk rst opts values = Circuit go
 where
  go (_, ~(_, acks)) = ((), (traceGroup name (traceStatus "status" status : monTraces), fwd))
   where
    (fwd, status) = unbundle (simMealy step state0 (bundle (unsafeToActiveHigh rst, acks)))
    (monTraces, _) = monitorCore rst fwd acks

  state0 =
    DriverState
      { items = values
      , idles = opts.idleCycles
      , driverWaiting = Nothing
      , nSent = 0
      , driverFailure = Nothing
      }

  -- Note that the data we offer may not depend on the acknowledgement of the
  -- current cycle, hence the lazy pattern match.
  step :: DriverState a -> (Bool, Ack) -> (DriverState a, (Maybe a, Status))
  step st ~(inReset, ack) =
    case (st.driverFailure, st.items) of
      (Just msg, _) -> (st, (Nothing, Error msg))
      (Nothing, []) -> (st, (Nothing, StopOk))
      (Nothing, x : rest)
        | inReset && not opts.driveDuringReset -> (st, (Nothing, PreventStop))
        | inReset ->
            let
              st1 = case isX ack of
                Right (Ack True) ->
                  st{driverFailure = Just (itemName <> " was acknowledged while in reset")}
                _ -> st{driverWaiting = Just 0}
             in
              (st1, (Just x, driverStatus st1))
        | otherwise ->
            let wait = fromMaybe (headOr0 st.idles) st.driverWaiting
             in if wait > 0
                  then (st{driverWaiting = Just (wait - 1)}, (Nothing, PreventStop))
                  else
                    let
                      st1 = case isX ack of
                        Left _ ->
                          st{driverFailure = Just ("acknowledgement of " <> itemName <> " is undefined")}
                        Right (Ack True) ->
                          st
                            { items = rest
                            , idles = L.drop 1 st.idles
                            , driverWaiting = Nothing
                            , nSent = st.nSent + 1
                            }
                        Right (Ack False) -> st{driverWaiting = Just 0}
                     in
                      (st1, (Just x, driverStatus st1))
   where
    itemName = "item #" <> showText st.nSent

  -- Errors caused by this cycle's acknowledgement are reported immediately. The
  -- status is not fed back into the design, so this is fine.
  driverStatus st = maybe PreventStop Error st.driverFailure

-- | What a 'sink' acknowledges when no data is offered
data IdleAck
  = -- | @Ack False@
    IdleNack
  | -- | @Ack True@
    IdleAck
  | -- | An undefined acknowledgement, see 'errorX'
    IdleUndefined
  | -- | Cycle through all of the above
    IdleCycle
  deriving (Eq, Show, Enum, Bounded)

-- | Options for 'sink'
data SinkOptions = SinkOptions
  { stallCycles :: [Natural]
  {- ^ Number of cycles to wait before acknowledging each value. Once the list
  runs out, values are acknowledged immediately.
  -}
  , idleAck :: IdleAck
  -- ^ What to acknowledge when no data is offered
  }
  deriving (Eq, Show)

-- | No stalls, 'IdleNack'
defSinkOptions :: SinkOptions
defSinkOptions = SinkOptions{stallCycles = [], idleAck = IdleNack}

data SinkState = SinkState
  { stalls :: [Natural]
  , sinkWaiting :: !(Maybe Natural)
  , nIdle :: !Int
  }

{- | Consume values, applying backpressure according to t'SinkOptions'. Does not
acknowledge anything during reset. A sink does not check anything by itself and
has no status, see 'expect' instead.
-}
sink ::
  forall dom a.
  (KnownDomain dom) =>
  -- | Name
  String ->
  Clock dom ->
  Reset dom ->
  SinkOptions ->
  Circuit (Df dom a) TraceTree
sink name _clk rst opts =
  Circuit $ \(fwd, _) -> (sinkCore rst opts fwd, traceGroup name [])

sinkCore ::
  forall dom a.
  (KnownDomain dom) =>
  Reset dom ->
  SinkOptions ->
  Signal dom (Maybe a) ->
  Signal dom Ack
sinkCore rst opts fwd =
  simMealy step state0 (bundle (unsafeToActiveHigh rst, fwd))
 where
  state0 = SinkState{stalls = opts.stallCycles, sinkWaiting = Nothing, nIdle = 0}

  step st ~(inReset, dat)
    | inReset = (st, Ack False)
    | otherwise =
        case isX dat of
          Right (Just _) ->
            let wait = fromMaybe (headOr0 st.stalls) st.sinkWaiting
             in if wait == 0
                  then (st{stalls = L.drop 1 st.stalls, sinkWaiting = Nothing}, Ack True)
                  else (st{sinkWaiting = Just (wait - 1)}, Ack False)
          _ -> (st{nIdle = st.nIdle + 1}, idleAck st.nIdle)

  idleAck n =
    case opts.idleAck of
      IdleNack -> Ack False
      IdleAck -> Ack True
      IdleUndefined -> errorX "sink: no acknowledgement while idle"
      IdleCycle -> case n `mod` 3 of
        0 -> Ack False
        1 -> Ack True
        _ -> errorX "sink: no acknowledgement while idle"

data MonitorState a = MonitorState
  { stalled :: !(Maybe a)
  , lastTransfer :: !Bool
  , stallRun :: !Int
  , monitorFailure :: !(Maybe Text)
  }

data MonitorOut a = MonitorOut
  { status :: Status
  , transaction :: Maybe a
  , transfer :: Bool
  , stall :: Bool
  , idle :: Bool
  , backToBack :: Bool
  , ackWhileIdle :: Bool
  , longStall :: Bool
  }

{- | Passively monitor a 'Df' channel. Its status is 'Error' when the channel
violates the 'Df' invariants:

  * data that is offered must stay the same until it is acknowledged;
  * data and acknowledgements may not be undefined while data is offered.

Its status is 'StopOk' otherwise. Also outputs the transferred values.
-}
monitor ::
  forall dom a.
  (KnownDomain dom, BitPack a, ShowX a) =>
  -- | Name
  String ->
  Clock dom ->
  Reset dom ->
  Circuit (Df dom a) (TraceTree, CSignal dom (Maybe a), Df dom a)
monitor name _clk rst = Circuit go
 where
  go (fwd, ~(_, _, acks)) = (acks, (traceGroup name traces, txs, fwd))
   where
    (traces, txs) = monitorCore rst fwd acks

monitorCore ::
  forall dom a.
  (KnownDomain dom, BitPack a, ShowX a) =>
  Reset dom ->
  Signal dom (Maybe a) ->
  Signal dom Ack ->
  ([TraceTree], Signal dom (Maybe a))
monitorCore rst fwd acks = (traces, (.transaction) <$> outs)
 where
  outs = simMealy monitorStep state0 (bundle (unsafeToActiveHigh rst, fwd, acks))
  state0 =
    MonitorState
      { stalled = Nothing
      , lastTransfer = False
      , stallRun = 0
      , monitorFailure = Nothing
      }
  traces =
    [ traceSignal "data" fwd
    , traceSignal "ack" acks
    , traceStatus "protocol" ((.status) <$> outs)
    , cover "transfer" ((.transfer) <$> outs)
    , cover "stall" ((.stall) <$> outs)
    , cover "idle" ((.idle) <$> outs)
    , cover "back_to_back" ((.backToBack) <$> outs)
    , cover "ack_while_idle" ((.ackWhileIdle) <$> outs)
    , cover "long_stall" ((.longStall) <$> outs)
    ]

monitorStep ::
  (BitPack a, ShowX a) =>
  MonitorState a ->
  (Bool, Maybe a, Ack) ->
  (MonitorState a, MonitorOut a)
monitorStep st ~(inReset, dat, ack)
  | inReset = (st{stalled = Nothing, lastTransfer = False, stallRun = 0}, quiet)
  | Just msg <- st.monitorFailure = (st, quiet{status = Error msg})
  | otherwise =
      case isX dat of
        Left _ -> failed "data is undefined"
        Right Nothing
          | Just p <- st.stalled ->
              failed ("data was withdrawn before it was acknowledged: " <> showX' p)
          | otherwise ->
              ( st{lastTransfer = False, stallRun = 0}
              , quiet{idle = True, ackWhileIdle = isX ack == Right (Ack True)}
              )
        Right (Just x) ->
          case (isX x, isX ack) of
            (Left _, _) -> failed "data is undefined while it is offered"
            (_, Left _) -> failed "acknowledgement is undefined while data is offered"
            (Right _, Right (Ack acked))
              | Just p <- st.stalled
              , packToSample p /= packToSample x ->
                  failed
                    ( "data changed before it was acknowledged: "
                        <> showX' p
                        <> " became "
                        <> showX' x
                    )
              | acked ->
                  ( st{stalled = Nothing, lastTransfer = True, stallRun = 0}
                  , quiet{transaction = Just x, transfer = True, backToBack = st.lastTransfer}
                  )
              | otherwise ->
                  let run = st.stallRun + 1
                   in ( st{stalled = Just x, lastTransfer = False, stallRun = run}
                      , quiet{stall = True, longStall = run >= 4}
                      )
 where
  failed msg = (st{monitorFailure = Just msg}, quiet{status = Error msg})
  showX' :: (ShowX b) => b -> Text
  showX' = Text.pack . showX
  quiet =
    MonitorOut
      { status = StopOk
      , transaction = Nothing
      , transfer = False
      , stall = False
      , idle = False
      , backToBack = False
      , ackWhileIdle = False
      , longStall = False
      }

-- | Options for 'expect', 'expectUnordered', and 'expectWith'
data ExpectOptions = ExpectOptions
  { sinkOptions :: SinkOptions
  -- ^ How to apply backpressure
  , settleCycles :: Natural
  {- ^ Keep the simulation running for this many cycles after the last
  transfer, to catch data that should not be there.
  -}
  }
  deriving (Eq, Show)

-- | 'defSinkOptions', settling for 16 cycles
defExpectOptions :: ExpectOptions
defExpectOptions = ExpectOptions{sinkOptions = defSinkOptions, settleCycles = 16}

{- | Consume values and check that they are equal to the given list, in order. Its
status is 'PreventStop' until all values have been received, and 'Error' as soon
as a value does not match, more values than expected arrive, or the 'Df'
invariants are violated (see 'monitor').
-}
expect ::
  forall dom a.
  (KnownDomain dom, BitPack a, ShowX a, Eq a) =>
  -- | Name
  String ->
  Clock dom ->
  Reset dom ->
  ExpectOptions ->
  -- | Expected values
  [a] ->
  Circuit (Df dom a) TraceTree
expect = expectBy InOrder

-- | Like 'expect', but values may arrive in any order
expectUnordered ::
  forall dom a.
  (KnownDomain dom, BitPack a, ShowX a, Eq a) =>
  -- | Name
  String ->
  Clock dom ->
  Reset dom ->
  ExpectOptions ->
  -- | Expected values
  [a] ->
  Circuit (Df dom a) TraceTree
expectUnordered = expectBy OutOfOrder

expectBy ::
  forall dom a.
  (KnownDomain dom, BitPack a, ShowX a, Eq a) =>
  Order ->
  String ->
  Clock dom ->
  Reset dom ->
  ExpectOptions ->
  [a] ->
  Circuit (Df dom a) TraceTree
expectBy order name clk rst opts values = Circuit go
 where
  go (fwd, _) = (acks, traceGroup name (sbTree : monTraces))
   where
    acks = sinkCore rst opts.sinkOptions fwd
    (monTraces, txs) = monitorCore rst fwd acks
    Circuit sb =
      scoreboard @dom @() @()
        ""
        clk
        ScoreboardOptions{order, settleCycles = opts.settleCycles}
        values
        ()
        (\s _ -> (s, []))
    (_, sbTree) = sb ((pure [], maybeToList <$> txs), ())

{- | Consume values and check them with a stateful predicate, see 'checker'. Use
this when the expected values cannot be determined up front.
-}
expectWith ::
  forall dom a s.
  (KnownDomain dom, BitPack a, ShowX a) =>
  -- | Name
  String ->
  Clock dom ->
  Reset dom ->
  ExpectOptions ->
  -- | Initial state
  s ->
  -- | Check a value
  (s -> a -> Either Text s) ->
  -- | Whether all expected values have been received
  (s -> Bool) ->
  Circuit (Df dom a) TraceTree
expectWith name clk rst opts state0 check done = Circuit go
 where
  go (fwd, _) = (acks, traceGroup name (checkTree : monTraces))
   where
    acks = sinkCore rst opts.sinkOptions fwd
    (monTraces, txs) = monitorCore rst fwd acks
    Circuit chk = checker "" clk opts.settleCycles state0 check done
    (_, checkTree) = chk (maybeToList <$> txs, ())

{- | Generate stalls: up to ten cycle counts. Half of them are zero, the others are
up to ten cycles. Used for @idleCycles@ and @stallCycles@.
-}
genStalls :: Gen [Natural]
genStalls =
  Gen.list
    (Range.linear 0 10)
    (Gen.frequency [(1, pure 0), (1, Gen.integral (Range.linear 1 10))])

-- | Generate t'DriverOptions'
genDriverOptions :: Gen DriverOptions
genDriverOptions = DriverOptions <$> genStalls <*> Gen.bool

-- | Generate t'SinkOptions'
genSinkOptions :: Gen SinkOptions
genSinkOptions = SinkOptions <$> genStalls <*> Gen.enumBounded

-- | Generate t'ExpectOptions', settling for 16 cycles
genExpectOptions :: Gen ExpectOptions
genExpectOptions = (\s -> ExpectOptions{sinkOptions = s, settleCycles = 16}) <$> genSinkOptions

headOr0 :: [Natural] -> Natural
headOr0 = fromMaybe 0 . listToMaybe
