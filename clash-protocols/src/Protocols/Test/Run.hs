{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NoFieldSelectors #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

module Protocols.Test.Run where

import Clash.Explicit.Prelude hiding ((:<))

import Clash.Signal.Internal (Femtoseconds (..))
import Control.Exception (Exception (..), catch, throwIO)
import Control.Monad (forM_, forM)
import Data.List.Infinite (Infinite ((:<)))
import Data.List.NonEmpty (NonEmpty ((:|)))
import Data.Text (Text)
import GHC.IOArray (newIOArray, IOArray, writeIOArray, readIOArray)
import Protocols.Internal (Circuit (..), toSignals)
import Protocols.Test.Driver (DriverSamples(..), Status (PreventStop, Error, StopOk))
import Protocols.Test.Samples (Samples(..), streamsToOrderedSamples, Sample)
import Data.Bifunctor (Bifunctor(second))

import qualified Data.List as L
import qualified Data.Map as Map
import qualified Data.List.NonEmpty as NonEmpty


data TestResult = TestResult
  { trace :: Maybe FilePath
  -- ^ If 'Nothing', no trace was produced. If 'Just', the path to the trace file.
  , result :: Maybe Text
  -- ^ If 'Nothing', test passed. If 'Just', test failed with the given message.
  , runTime :: Femtoseconds
  -- ^ Simulation time taken to run the test
  }
  deriving (Eq, Show)

testResult :: TestResult
testResult = TestResult
  { trace = Nothing
  , result = Nothing
  , runTime = Femtoseconds 0
  }

instance Exception TestResult where
  displayException = show


data TestOptions = TestOptions
  { trace :: Bool
  -- ^ Whether to produce a trace file
  , timeout :: Maybe Femtoseconds
  -- ^ Timeout in simulation time
  }
  deriving (Eq, Show)

defTestOptions :: TestOptions
defTestOptions = TestOptions
  { trace = False
  , timeout = Nothing
  }

vecToNonEmpty :: forall n a. (1 <= n) => Vec n a -> NonEmpty a
vecToNonEmpty v = leToPlus @1 @n $ head v :| toList (tail v)

runTest ::
  (1 <= n, KnownNat n) =>
  TestOptions ->
  Circuit () (Vec n DriverSamples) ->
  IO TestResult
runTest _options dut = do
  statusArray <- newIOArray (0, nDrivers - 1) PreventStop
  catch (go statusArray allSamples) pure
 where
  nDrivers = L.length driverSamples

  traces = [(s.name, t) | s <- NonEmpty.toList driverSamples, t <- s.traces]
  traceNames = second (.name) <$> traces
  traceIdMap = Map.fromList $ L.zip traceNames [0..]

  driverNames = NonEmpty.toList $ (.name) <$> driverSamples
  driverNameMap = Map.fromList $ L.zip [0..] driverNames
  driverIdMap = Map.fromList $ L.zip driverNames [0..]

  allSamples = streamsToOrderedSamples allStreams
  driverSamples = vecToNonEmpty $ snd $ toSignals dut ((), repeat ())
  statusStreams = (\s -> (driverIdMap Map.! s.name, second Left <$> s.statuses)) <$> driverSamples
  traceStreams = [(traceIdMap Map.! (nm, s.name), second Right <$> s.samples) | (nm, s) <- traces]
  allStreams = NonEmpty.prependList traceStreams statusStreams

  go ::
    IOArray Int Status ->
    Infinite (Femtoseconds, NonEmpty (Int, Either Status Sample)) ->
    IO TestResult
  go statusArray ((timestamp, statuses) :< future) = do
    -- Check samples at current timestamp
    forM_ statuses $ \(id_, status) -> do
      case status of
        Left s ->
          -- Update status array with the current statuses
          writeIOArray statusArray id_ s
        Right _s -> do
          -- TODO: Update trace. I.e., write to a VCD file.
          pure ()

    -- Check for errors / termination conditions
    dones <- forM [0..nDrivers - 1] $ \i -> do
      status <- readIOArray statusArray i
      case status of
        PreventStop ->
          -- Not done yet
          pure False

        StopOk -> do
          -- Done!
          pure True

        Error msg -> do
          -- Immediately exit if an error is detected
          throwIO $ TestResult
            { trace = Nothing
            , result = Just $ driverNameMap Map.! i <> ": " <> msg
            , runTime = timestamp
            }

    if L.and dones
    then pure $ testResult{runTime = timestamp}
    else go statusArray future
