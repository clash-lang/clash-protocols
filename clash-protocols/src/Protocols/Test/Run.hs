{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoFieldSelectors #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Protocols.Test.Run where

import Clash.Explicit.Prelude hiding (sample, (:<))

import Control.Exception (Exception (..), throw)
import Control.Monad (forM_, when)
import Control.Monad.Extra (whenM)
import Data.Bifunctor (Bifunctor (first, second))
import Data.IORef (IORef)
import Data.List.Infinite (Infinite ((:<)))
import Data.List.NonEmpty (NonEmpty ((:|)))
import Data.Text (Text)
import GHC.Arr (Array)
import GHC.IOArray (IOArray)
import Protocols.Test.Samples (
  HectoFemtoseconds,
  Sample (Sample),
  StatusSample (Error, PreventStop, StopOk),
  Trace (samples),
  TraceTree,
  flattenTraceTree,
  isStatusSamples,
  samplesWidth,
  statusSampleToSample,
  toOrderedSamples,
  traceWidth,
 )
import Protocols.Test.Samples.Vcd (
  HierarchicalName,
  Scope,
  VcdHeader (..),
  renderHeader,
  renderSample,
  renderStatusSample,
  toHierarchicalName,
  toScopes,
 )
import System.IO (Handle, IOMode (WriteMode), hPutStrLn, withFile)

import Data.IORef qualified as IoRef
import Data.List qualified as L
import Data.List.Infinite qualified as Infinite
import Data.List.NonEmpty qualified as NonEmpty
import Data.Map qualified as Map
import Data.Text qualified as Text
import Data.Text.IO qualified as TextIo
import GHC.Arr qualified as Array
import GHC.IOArray qualified as IoArray

data TestResult = TestResult
  { trace :: Maybe FilePath
  -- ^ If 'Nothing', no trace was produced. If 'Just', the path to the trace file.
  , result :: Maybe Text
  -- ^ If 'Nothing', test passed. If 'Just', test failed with the given message.
  , runTime :: HectoFemtoseconds
  -- ^ Simulation time taken to run the test
  }
  deriving (Eq, Show)

testResult :: TestResult
testResult =
  TestResult
    { trace = Nothing
    , result = Nothing
    , runTime = 0
    }

instance Exception TestResult where
  displayException = show

data TestOptions = TestOptions
  { trace :: Bool
  -- ^ Whether to produce a trace file
  , timeout :: Maybe HectoFemtoseconds
  -- ^ Timeout in simulation time
  }
  deriving (Eq, Show)

defTestOptions :: TestOptions
defTestOptions =
  TestOptions
    { trace = False
    , timeout = Nothing
    }

vecToNonEmpty :: forall n a. (1 <= n) => Vec n a -> NonEmpty a
vecToNonEmpty v = leToPlus @1 @n $ head v :| toList (tail v)

newIoArrayFromList :: e -> [(Int, e)] -> IO (IOArray Int e)
newIoArrayFromList dflt xs = do
  array <- IoArray.newIOArray (0, L.maximum (fst <$> xs)) dflt
  mapM_ (uncurry $ IoArray.writeIOArray array) xs
  pure array

ioArrayToList :: IOArray Int e -> IO [(Int, e)]
ioArrayToList array = do
  let (l, h) = IoArray.boundsIOArray array
  mapM (\i -> (i,) <$> IoArray.readIOArray array i) [l .. h]

arrayFromList :: [e] -> Array Int e
arrayFromList es = Array.listArray (0, L.length es - 1) es

checkStatus :: IOArray Int StatusSample -> IO (IORef Int)
checkStatus status0 = do
  n <- IoRef.newIORef (0 :: Int)
  let (l, h) = IoArray.boundsIOArray status0
  forM_ [l .. h] $ \i -> do
    IoArray.readIOArray status0 i >>= \case
      StopOk -> pure ()
      PreventStop -> IoRef.modifyIORef n succ
      Error e -> throw (testResult{result = Just (Text.pack e)})
  pure n

run# ::
  TestOptions ->
  NonEmpty (HierarchicalName, Trace) ->
  IO TestResult
run# options traces0 = do
  statusSamples <- newIoArrayFromList StopOk firstStatusSamples
  traceSamples <- newIoArrayFromList (Sample 0 0) firstTraceSamples
  labels <- newIoArrayFromList "INTERNAL_ERROR" (Map.toList labelMap1)
  widths <- newIoArrayFromList (-1) (Map.toList widthMap)
  nLeft <- checkStatus statusSamples
  IoRef.readIORef nLeft >>= \case
    0 -> pure testResult
    _ ->
      if options.trace
        then withFile "trace.vcd" WriteMode $ \traceHandle -> do
          runWithTrace#
            traceHandle
            options
            nLeft
            traceSamples
            statusSamples
            labels
            widths
            scopes
            restSamples
        else
          runNoTrace#
            options
            nLeft
            statusSamples
            (second fst <$> restSamples)
 where
  traces1
    | options.trace = traces0
    | otherwise = case NonEmpty.nonEmpty onlyStatusTraces of
        Nothing -> throw (testResult{result = Just "No status traces provided"})
        Just xs -> xs
   where
    onlyStatusTraces =
      let predicate (_, t) = isStatusSamples t.samples
       in L.filter predicate (NonEmpty.toList traces0)

  indexedTraces = NonEmpty.zip (NonEmpty.fromList [0 ..]) (snd <$> traces1)
  allOrderedSamples = toOrderedSamples indexedTraces
  (snd -> firstSamples, restSamples) = Infinite.uncons allOrderedSamples
  (firstStatusSamples, firstTraceSamples) = firstSamples

  -- For traces:
  widthMap = Map.fromList $ second traceWidth <$> NonEmpty.toList indexedTraces
  indexMap = Map.fromList $ L.zip (NonEmpty.toList $ fst <$> traces1) [0 ..]
  labelMap1 = Map.fromList $ (\(nm, i) -> (i, labelMap0 Map.! nm)) <$> Map.toList indexMap
  firstStatusSamples1 = fmap (second statusSampleToSample) firstStatusSamples
  initSamples = Map.fromList (firstStatusSamples1 <> firstTraceSamples)
  toScopesInput nm t = (nm, samplesWidth t.samples, initVal t.samples nm)
  (scopes, labelMap0) = toScopes (uncurry toScopesInput <$> NonEmpty.toList traces1)
  initVal samples nm =
    renderSample
      (samplesWidth samples)
      (labelMap0 Map.! nm)
      (initSamples Map.! (indexMap Map.! nm))

updateNLeft :: HectoFemtoseconds -> IORef Int -> StatusSample -> StatusSample -> IO ()
updateNLeft hfs nLeft prevStatus status =
  case (prevStatus, status) of
    (PreventStop, StopOk) -> IoRef.modifyIORef nLeft ((-) 1)
    (StopOk, PreventStop) -> IoRef.modifyIORef nLeft (+ 1)
    (_, Error e) -> throw (testResult{runTime = hfs, result = Just (Text.pack e)})
    _ -> error "Should be unreachable: status change not handled?"
{-# INLINE updateNLeft #-}

runWithTrace# ::
  Handle ->
  TestOptions ->
  IORef Int ->
  IOArray Int Sample ->
  IOArray Int StatusSample ->
  IOArray Int Text ->
  IOArray Int Int ->
  [Scope] ->
  Infinite (HectoFemtoseconds, ([(Int, StatusSample)], [(Int, Sample)])) ->
  IO TestResult
runWithTrace# traceHandle options nLeft sampleArray statusArray labels widths scopes trace0 = do
  TextIo.hPutStr traceHandle header
  firstChange <- IoRef.newIORef True
  go firstChange trace0
 where
  header =
    renderHeader
      VcdHeader
        { scopes = scopes
        , timescale = "100fs"
        , date = "1970-01-01"
        , version = "clash-protocols-run-0.1"
        }

  writeTime firstChange hfs =
    whenM (IoRef.readIORef firstChange) $ do
      hPutStrLn traceHandle ('#' : show hfs)
      IoRef.writeIORef firstChange False

  go firstChange ((hfs, (statusSamples, samples)) :< rest) = do
    IoRef.writeIORef firstChange True

    -- Status samples
    forM_ statusSamples $ \(ix, status) -> do
      prevStatus <- IoArray.readIOArray statusArray ix
      when (prevStatus /= status) $ do
        updateNLeft hfs nLeft prevStatus status
        writeTime firstChange hfs
        label <- IoArray.readIOArray labels ix
        TextIo.hPutStr traceHandle (renderStatusSample label status)
        IoArray.writeIOArray statusArray ix status

    -- Samples for traces
    forM_ samples $ \(ix, sample) -> do
      prevSample <- IoArray.readIOArray sampleArray ix
      when (prevSample /= sample) $ do
        writeTime firstChange hfs
        label <- IoArray.readIOArray labels ix
        width <- IoArray.readIOArray widths ix
        TextIo.hPutStr traceHandle (renderSample width label sample)
        IoArray.writeIOArray sampleArray ix sample

    -- Check whether we can terminate the simulation
    IoRef.readIORef nLeft >>= \case
      0 -> pure $ testResult{runTime = hfs, result = Nothing}
      _ -> case options.timeout of
        Nothing -> go firstChange rest
        Just timeoutHfs ->
          if hfs >= timeoutHfs
            then throw (testResult{runTime = hfs, result = Just "Timeout reached"})
            else go firstChange rest

runNoTrace# ::
  TestOptions ->
  IORef Int ->
  IOArray Int StatusSample ->
  Infinite (HectoFemtoseconds, [(Int, StatusSample)]) ->
  IO TestResult
runNoTrace# options nLeft statusArray = go
 where
  go ((hfs, traces) :< rest) = do
    forM_ traces $ \(ix, status) -> do
      prevStatus <- IoArray.readIOArray statusArray ix
      when (prevStatus /= status) $ do
        updateNLeft hfs nLeft prevStatus status
        IoArray.writeIOArray statusArray ix status

    IoRef.readIORef nLeft >>= \case
      0 -> pure $ testResult{runTime = hfs, result = Nothing}
      _ -> case options.timeout of
        Nothing -> go rest
        Just timeoutHfs ->
          if hfs >= timeoutHfs
            then throw (testResult{runTime = hfs, result = Just "Timeout reached"})
            else go rest

run ::
  TestOptions ->
  TraceTree ->
  IO TestResult
run options traceTree =
  maybe
    (throw (testResult{result = Just "No traces provided"}))
    (run# options . fmap (first toHierarchicalName))
    (flattenTraceTree traceTree)
