{-# LANGUAGE OverloadedStrings #-}
-- KnownDomain Fast
{-# OPTIONS_GHC -Wno-orphans #-}

-- | Tests of the runner in "Protocols.Experimental.Testbench.Run"
module Tests.Protocols.Testbench.Run where

import Prelude

import Clash.Prelude (Signal, System)
import Clash.Prelude qualified as C
import Control.Monad (forM_)
import Data.Map qualified as Map
import Data.Text qualified as Text
import Protocols.Experimental.Testbench
import System.Directory (doesFileExist, getTemporaryDirectory, removeFile)
import System.FilePath ((</>))
import Test.Tasty (TestTree)
import Test.Tasty.HUnit (Assertion, assertBool, testCase, (@?=))
import Test.Tasty.TH (testGroupGenerator)

C.createDomain C.vSystem{C.vName = "Fast", C.vPeriod = 3000}

-- | 'PreventStop' for the given number of cycles, 'StopOk' afterwards
stopAfter :: (C.KnownDomain dom) => Int -> Signal dom Status
stopAfter n = C.fromList_lazy (replicate n PreventStop <> repeat StopOk)

-- | A testbench consisting of status traces in the 'System' domain
statuses :: [(String, Signal System Status)] -> TraceTree
statuses ss = traceGroup "tb" [traceStatus name s | (name, s) <- ss]

run :: TraceTree -> IO TestResult
run = runTraceTree defTestOptions{timeout = Just (cycles @System 1000)}

case_noStatus :: Assertion
case_noStatus = do
  r <- run (traceGroup "tb" [traceSignal @System "x" (pure (0 :: C.Unsigned 4))])
  r.outcome @?= Failed{source = "", message = "testbench has no status traces"}

case_stopImmediately :: Assertion
case_stopImmediately = do
  r <- run (statuses [("a", pure StopOk)])
  r.outcome @?= Passed
  r.endTime @?= 0

-- | The simulation stops as soon as all statuses are 'StopOk'
case_stopWhenAllStopOk :: Assertion
case_stopWhenAllStopOk =
  forM_ [[3], [3, 5], [7, 3, 5], [4, 4]] $ \ns -> do
    r <- run (statuses [(show i, stopAfter n) | (i, n) <- zip [0 :: Int ..] ns])
    r.outcome @?= Passed
    r.endTime @?= cycles @System (fromIntegral (maximum ns))

-- | Statuses may go back to 'PreventStop'
case_preventStopAgain :: Assertion
case_preventStopAgain = do
  let a =
        C.fromList_lazy
          ([PreventStop, StopOk, PreventStop, PreventStop, PreventStop, PreventStop] <> repeat StopOk)
  r <- run (statuses [("a", a), ("b", stopAfter 3)])
  r.outcome @?= Passed
  r.endTime @?= cycles @System 6

case_error :: Assertion
case_error = do
  let a = C.fromList_lazy (replicate 3 PreventStop <> repeat (Error "boom"))
  r <- run (statuses [("a", a), ("b", stopAfter 10)])
  r.outcome @?= Failed{source = "tb/a", message = "boom"}
  r.endTime @?= cycles @System 3

case_timeout :: Assertion
case_timeout = do
  r <- run (statuses [("a", stopAfter 2), ("b", pure PreventStop)])
  r.outcome @?= TimedOut{blockedBy = ["tb/b"]}
  r.endTime @?= cycles @System 1000

case_undefinedStatus :: Assertion
case_undefinedStatus = do
  let a = C.fromList_lazy (PreventStop : C.errorX "undefined" : repeat StopOk)
  r <- run (statuses [("a", a)])
  r.outcome @?= Failed{source = "tb/a", message = "status is undefined"}
  r.endTime @?= cycles @System 1

-- | Exceptions other than 'C.XException's end the simulation
case_exception :: Assertion
case_exception = do
  let a = C.fromList_lazy (PreventStop : PreventStop : error "kaboom")
  r <- run (statuses [("a", a)])
  case r.outcome of
    Failed{source = "", message} -> assertBool "message" ("kaboom" `Text.isInfixOf` message)
    outcome -> assertBool ("unexpected outcome: " <> show outcome) False

-- | Traces in domains with different periods are simulated in time order
case_multipleDomains :: Assertion
case_multipleDomains =
  forM_ [(10, 2, 30), (5, 2, 20), (4, 4, 40)] $ \(nFast, nSystem, endNs) -> do
    r <-
      run $
        traceGroup
          "tb"
          [ traceStatus @Fast "fast" (stopAfter nFast)
          , traceStatus @System "system" (stopAfter nSystem)
          ]
    r.outcome @?= Passed
    r.endTime @?= endNs * 1_000_000

-- | Writing a waveform does not change the outcome of a simulation
case_vcdOutput :: Assertion
case_vcdOutput = do
  tmp <- getTemporaryDirectory
  let
    path = tmp </> "clash-protocols-testbench-run-test.vcd"
    passing = statuses [("a", stopAfter 3)]
    failing = statuses [("a", C.fromList_lazy (PreventStop : repeat (Error "boom")))]
    withVcd vcd = runTraceTree defTestOptions{vcdOutput = vcd}

  r0 <- withVcd NoVcd passing
  r1 <- withVcd (VcdAlways path) passing
  r1.vcdFile @?= Just path
  r1{vcdFile = Nothing} @?= r0
  removeFile path

  r2 <- withVcd (VcdOnFailure path) passing
  r2.vcdFile @?= Nothing
  doesFileExist path >>= assertBool "no waveform on success" . not

  r3 <- withVcd (VcdOnFailure path) failing
  r3.vcdFile @?= Just path
  r3.outcome @?= Failed{source = "tb/a", message = "boom"}
  doesFileExist path >>= assertBool "waveform on failure"
  removeFile path

-- | Long simulations work, see the README for memory use
case_longSimulation :: Assertion
case_longSimulation = do
  let
    n = 100_000
    counter = C.fromList_lazy (cycle [0 .. 15 :: C.Unsigned 4])
  r <-
    runTraceTree
      defTestOptions
      ( traceGroup
          "tb"
          [ traceStatus @System "status" (stopAfter n)
          , traceSignal @System "counter" counter
          , cover @System "zero" ((== 0) <$> counter)
          ]
      )
  r.outcome @?= Passed
  r.endTime @?= cycles @System (fromIntegral n)
  r.coverage @?= Map.fromList [("tb/zero", fromIntegral (n `div` 16 + 1))]

tests :: TestTree
tests = $(testGroupGenerator)
