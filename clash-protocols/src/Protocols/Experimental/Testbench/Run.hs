{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoFieldSelectors #-}

{- |
The runner: evaluates a testbench, a t'Circuit' from @()@ to 'TraceTree', in
simulation time order until all its status traces allow it to stop, one of them
reports an error, or a timeout is reached.
-}
module Protocols.Experimental.Testbench.Run (
  -- * Running testbenches
  runTestbench,
  runTraceTree,
  simulate,

  -- * Options
  TestOptions (..),
  defTestOptions,
  VcdOutput (..),

  -- * Results
  TestResult (..),
  Outcome (..),
) where

import Clash.Prelude (ActiveEdge (..), SimOnly (..), isX)
import Control.DeepSeq (NFData (rnf), force)
import Control.Exception (
  SomeAsyncException,
  SomeException,
  displayException,
  evaluate,
  fromException,
  throwIO,
  try,
 )
import Control.Monad (forM, forM_, when)
import Data.Foldable (toList)
import Data.IORef (IORef, modifyIORef', newIORef, readIORef, writeIORef)
import Data.List qualified as L
import Data.List.Infinite (Infinite ((:<)))
import Data.List.Infinite qualified as I
import Data.List.NonEmpty (NonEmpty ((:|)))
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (isJust)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.IO qualified as TextIO
import GHC.Arr (listArray, (!))
import GHC.IOArray (newIOArray, readIOArray, writeIOArray)
import Numeric.Natural (Natural)
import Protocols.Experimental.Testbench.Trace
import Protocols.Experimental.Testbench.Vcd (
  VcdVar (..),
  identifiers,
  renderHeader,
  renderSample,
 )
import Protocols.Plugin (Circuit (..))
import System.IO (
  BufferMode (BlockBuffering),
  IOMode (WriteMode),
  hSetBuffering,
  withFile,
 )

-- | Whether and when to write a waveform (VCD) file
data VcdOutput
  = -- | Never write a waveform
    NoVcd
  | {- | Write a waveform to the given path, but only if the test did not pass.
    The simulation is first run without tracing. If it does not pass, it is run
    again with tracing enabled. Because the second run needs the testbench, the
    simulation history is kept in memory during the first run. Use 'NoVcd' or
    'VcdAlways' for very long simulations.
    -}
    VcdOnFailure FilePath
  | -- | Always write a waveform to the given path
    VcdAlways FilePath
  deriving (Eq, Show)

-- | Options for 'runTestbench'
data TestOptions = TestOptions
  { vcdOutput :: VcdOutput
  -- ^ Whether to write a waveform
  , timeout :: Maybe Femtoseconds
  {- ^ Stop the simulation, and report 'TimedOut', once this much simulation time
  has passed. See 'cycles' to express it in clock cycles. Without a timeout, a
  testbench that never stops simulates forever.
  -}
  }
  deriving (Eq, Show)

-- | Default options: no waveform, no timeout
defTestOptions :: TestOptions
defTestOptions = TestOptions{vcdOutput = NoVcd, timeout = Nothing}

-- | How a simulation ended
data Outcome
  = -- | All status traces said 'StopOk'
    Passed
  | -- | A status trace said 'Error', or the simulation threw an exception
    Failed
      { source :: Text
      -- ^ Path of the status trace that reported the error, see 'TraceTree'
      , message :: Text
      -- ^ The error
      }
  | -- | The timeout was reached
    TimedOut
      { blockedBy :: [Text]
      -- ^ Paths of the status traces that still said 'PreventStop'
      }
  deriving (Eq, Show)

-- | Result of 'runTestbench'
data TestResult = TestResult
  { outcome :: Outcome
  -- ^ How the simulation ended
  , endTime :: Femtoseconds
  -- ^ Simulation time at which the simulation ended
  , coverage :: Map Text Natural
  -- ^ Number of cycles each cover point was hit, keyed by path
  , vcdFile :: Maybe FilePath
  -- ^ Path of the waveform, if one was written
  }
  deriving (Eq, Show)

-- | Run a testbench. See t'TestOptions'.
runTestbench :: TestOptions -> Circuit () TraceTree -> IO TestResult
runTestbench opts (Circuit f) = runTraceTree opts (snd (f ((), ())))

-- | Like 'runTestbench', but takes the trace tree directly
runTraceTree :: TestOptions -> TraceTree -> IO TestResult
runTraceTree opts tree =
  case opts.vcdOutput of
    NoVcd -> simulate Nothing opts.timeout tree
    VcdAlways path -> withVcd path
    VcdOnFailure path -> do
      result <- simulate Nothing opts.timeout tree
      case result.outcome of
        Passed -> pure result
        _ -> withVcd path
 where
  withVcd path =
    withFile path WriteMode $ \h -> do
      hSetBuffering h (BlockBuffering Nothing)
      result <- simulate (Just (TextIO.hPutStr h)) opts.timeout tree
      pure result{vcdFile = Just path}

data Kind = KSignal | KClock | KStatus | KCover
  deriving (Eq)

{- | Everything the runner needs to know about a trace, except its samples. Its
fields (and those of t'VcdVar') are strict, because a lazy field would refer to
its t'Trace', which in turn refers to the start of its samples. That would keep
the complete simulation history in memory.
-}
data Meta = Meta
  { path :: !Text
  , kind :: !Kind
  , width :: !Int
  , var :: !VcdVar
  }

instance NFData Meta where
  rnf Meta{path, kind, width, var} =
    rnf path `seq` kind `seq` rnf width `seq` rnf (var.scopes, var.name, var.identifier)

-- | A sample of any kind of trace
data Value
  = VSample Sample
  | VStatus Status
  | VCover Bool

type Events = Infinite (Femtoseconds, [(Int, Value)])

{- | Simulate a trace tree. If a sink is given, a waveform is written to it. This
is the function underlying 'runTestbench', exposed for testing purposes.
-}
simulate ::
  -- | Where to write a waveform to, if anywhere
  Maybe (Text -> IO ()) ->
  -- | Timeout
  Maybe Femtoseconds ->
  TraceTree ->
  IO TestResult
simulate mSink timeout tree = do
  (metas, events0) <- prepare (isJust mSink) tree
  -- The events are only ever referenced through this 'IORef', which the
  -- simulation loop advances. This makes sure no part of the simulation history
  -- is retained, so simulations run in constant memory.
  eventsRef <- newIORef events0
  runEvents mSink timeout metas eventsRef

{- | Flatten a trace tree into the information the runner needs about each trace,
and a stream of events. The trace tree itself, and therefore the start of every
stream, is not retained.
-}
prepare :: Bool -> TraceTree -> IO ([Meta], Maybe Events)
prepare tracing (SimOnly tree) = do
  let selected = [(segs, t) | (segs, t) <- dedupe (flatten tree), tracing || isControl t]
  metas <- evaluate (force (L.zipWith mkMeta identifiers selected))
  streams <- evaluate (forceSpine [(i, toStream t) | (i, (_, t)) <- L.zip [0 ..] selected])
  pure
    (metas, if L.null streams then Nothing else Just (mergeStreams (groupByStep streams)))
 where
  forceSpine xs = L.foldr (\(i, (step, s)) acc -> i `seq` step `seq` s `seq` acc) () xs `seq` xs
{-# NOINLINE prepare #-}

runEvents ::
  Maybe (Text -> IO ()) ->
  Maybe Femtoseconds ->
  [Meta] ->
  IORef (Maybe Events) ->
  IO TestResult
runEvents mSink timeout metas eventsRef = do
  let
    tracing = isJust mSink
    nTraces = L.length metas
    metaArr = L.zip [0 :: Int ..] metas
    statusIxs = [i | (i, m) <- metaArr, m.kind == KStatus]
    coverIxs = [i | (i, m) <- metaArr, m.kind == KCover]
    metaLookup = listArray (0, nTraces - 1) metas
    pathOf i = (metaLookup ! i).path
  case statusIxs of
    [] ->
      pure
        TestResult
          { outcome = Failed{source = "", message = "testbench has no status traces"}
          , endTime = 0
          , coverage = Map.empty
          , vcdFile = Nothing
          }
    _ -> do
      statuses <- newIOArray (0, nTraces - 1) StopOk
      prevSamples <- newIOArray (0, nTraces - 1) (Sample 0 0)
      prevCovers <- newIOArray (0, nTraces - 1) False
      coverCounts <- newIOArray (0, nTraces - 1) (0 :: Natural)
      nPrevent <- newIORef (0 :: Int)
      firstError <- newIORef Nothing
      timeRef <- newIORef 0

      forM_ mSink $ \sink -> sink (renderHeader [m.var | m <- metas])

      let
        coverageMap = do
          counts <- forM coverIxs (readIOArray coverCounts)
          pure (Map.fromList (L.zip (pathOf <$> coverIxs) counts))

        result outcome t = do
          cov <- coverageMap
          pure TestResult{outcome, endTime = t, coverage = cov, vcdFile = Nothing}

        processEvent :: Bool -> (Int, Value) -> IO (Maybe Text)
        processEvent isFirst (i, value) =
          case value of
            VStatus status0 -> do
              let status = normalizeStatus status0
              prev <- readIOArray statuses i
              let changed = status /= prev
              when changed $ do
                writeIOArray statuses i status
                when (prev == PreventStop) $ modifyIORef' nPrevent (subtract 1)
                when (status == PreventStop) $ modifyIORef' nPrevent (+ 1)
                case status of
                  Error msg ->
                    readIORef firstError >>= \case
                      Nothing -> writeIORef firstError (Just (i, msg))
                      Just _ -> pure ()
                  _ -> pure ()
              vcdChange (changed || isFirst) i (statusToSample status)
            VCover hit0 -> do
              let hit = either (const False) id (isX hit0)
              when hit $ do
                n <- readIOArray coverCounts i
                writeIOArray coverCounts i $! n + 1
              prev <- readIOArray prevCovers i
              let changed = hit /= prev
              when changed $ writeIOArray prevCovers i hit
              vcdChange (changed || isFirst) i (boolToSample hit)
            VSample sample -> do
              prev <- readIOArray prevSamples i
              let changed = sample /= prev
              when changed $ writeIOArray prevSamples i sample
              vcdChange (changed || isFirst) i sample

        vcdChange :: Bool -> Int -> Sample -> IO (Maybe Text)
        vcdChange write i sample
          | tracing && write = do
              let m = metaLookup ! i
              pure (Just (renderSample m.width m.var.identifier sample))
          | otherwise = pure Nothing

        loop :: Bool -> IO TestResult
        loop isFirst = do
          (t, events) <-
            readIORef eventsRef >>= \case
              Just ((t, events) :< rest) -> do
                writeIORef eventsRef (Just rest)
                pure (t, events)
              Nothing -> error "simulate: no events"
          writeIORef timeRef $! t
          changes <- forM events (processEvent isFirst)
          forM_ mSink $ \sink ->
            case [c | Just c <- changes] of
              [] | not isFirst -> pure ()
              cs
                | isFirst -> sink ("#0\n$dumpvars\n" <> mconcat cs <> "$end\n")
                | otherwise -> sink ("#" <> Text.pack (show t) <> "\n" <> mconcat cs)
          readIORef firstError >>= \case
            Just (i, msg) -> result Failed{source = pathOf i, message = msg} t
            Nothing -> do
              n <- readIORef nPrevent
              case timeout of
                _ | n == 0 -> result Passed t
                Just timeoutFs | t >= timeoutFs -> do
                  blocked <- forM statusIxs $ \i -> (i,) <$> readIOArray statuses i
                  result TimedOut{blockedBy = [pathOf i | (i, PreventStop) <- blocked]} t
                _ -> loop False

      try (loop True) >>= \case
        Right res -> pure res
        Left (e :: SomeException)
          | Just (_ :: SomeAsyncException) <- fromException e -> throwIO e
          | otherwise -> do
              t <- readIORef timeRef
              result Failed{source = "", message = Text.pack (displayException e)} t

mkMeta :: Text -> ([Text], Trace) -> Meta
mkMeta ident (segs, trace) =
  Meta
    { path = Text.intercalate "/" segs
    , kind
    , width
    , var =
        VcdVar
          { scopes = L.init segs
          , name = L.last segs
          , identifier = ident
          , width
          }
    }
 where
  (kind, width) = case trace.samples of
    SignalSamples w _ -> (KSignal, w)
    ClockSamples _ -> (KClock, 1)
    StatusSamples _ -> (KStatus, 2)
    CoverSamples _ -> (KCover, 1)

{- | Treat undefined statuses as errors. In waveforms, statuses show up as a 2-bit
value: 0 for 'PreventStop', 1 for 'StopOk', and 2 for 'Error'.
-}
normalizeStatus :: Status -> Status
normalizeStatus status =
  case isX status of
    Left _ -> Error "status is undefined"
    Right (Error msg) | Left _ <- isX msg -> Error "status is an error with an undefined message"
    Right s -> s

statusToSample :: Status -> Sample
statusToSample = \case
  PreventStop -> Sample 0 0
  StopOk -> Sample 0 1
  Error _ -> Sample 0 2

boolToSample :: Bool -> Sample
boolToSample b = Sample 0 (if b then 1 else 0)

-- | Flatten a trace tree into paths and traces. Empty names are skipped.
flatten :: TraceTreeI -> [([Text], Trace)]
flatten = go []
 where
  go prefix TraceTreeI{name, traces} = L.concatMap (either leaf node) traces
   where
    prefix1 = prefix <> segment name
    leaf trace = [(prefix1 <> [nonEmpty (Text.pack trace.name)], trace)]
    node (SimOnly sub) = go prefix1 sub

  segment n = [Text.pack n | not (L.null n)]
  nonEmpty n = if Text.null n then "unnamed" else n

-- | Make all paths unique by adding a suffix to duplicates, and replace slashes
dedupe :: [([Text], a)] -> [([Text], a)]
dedupe = snd . L.mapAccumL go Set.empty . fmap (\(segs, a) -> (fmap sanitize segs, a))
 where
  sanitize = Text.replace "/" "_"
  joinPath = Text.intercalate "/"

  go seen (segs, a)
    | Set.notMember (joinPath segs) seen = (Set.insert (joinPath segs) seen, (segs, a))
    | otherwise = go seen (L.init segs <> [suffixed (L.last segs)], a)
   where
    suffixed n =
      case L.find (\n1 -> Set.notMember (joinPath (L.init segs <> [n1])) seen) (candidates n) of
        Just n1 -> n1
        Nothing -> error "dedupe: unreachable"
    candidates n = [n <> "_" <> Text.pack (show k) | k <- [2 :: Int ..]]

-- | Convert a trace to its samples and the time between two samples
toStream :: Trace -> (Femtoseconds, Infinite Value)
toStream trace =
  case trace.samples of
    SignalSamples _ s -> (trace.period, VSample <$> s)
    StatusSamples s -> (trace.period, VStatus <$> s)
    CoverSamples s -> (trace.period, VCover <$> s)
    ClockSamples edge ->
      ( trace.period `div` 2
      , VSample <$> I.cycle (case edge of Rising -> one :| [zero]; Falling -> zero :| [one])
      )
 where
  zero = Sample 0 0
  one = Sample 0 1

-- | Group streams with the same time between samples, so they can be stepped together
groupByStep :: [(Int, (Femtoseconds, Infinite Value))] -> NonEmpty Events
groupByStep streams =
  case Map.toList (Map.fromListWith (flip (<>)) [(step, [(i, s)]) | (i, (step, s)) <- streams]) of
    [] -> error "groupByStep: no streams"
    (g : gs) -> uncurry groupStream <$> (g :| gs)
 where
  groupStream :: Femtoseconds -> [(Int, Infinite Value)] -> Events
  groupStream step = go 0
   where
    -- Timestamps are forced, as they are not necessarily inspected by the runner
    go !t ss = (t, [(i, I.head s) | (i, s) <- ss]) :< go (t + step) [(i, I.tail s) | (i, s) <- ss]

-- | Merge streams of events into a single stream, ordered by time
mergeStreams :: NonEmpty Events -> Events
mergeStreams (s :| []) = s
mergeStreams streams = (tMin, L.concatMap snd now) :< mergeStreams (advance <$> streams)
 where
  heads = I.head <$> streams
  tMin = L.minimum (fst <$> heads)
  now = L.filter ((== tMin) . fst) (toList heads)
  advance s = if fst (I.head s) == tMin then I.tail s else s
