{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fplugin Protocols.Plugin #-}

{- | Tests that the 'Df' components of "Protocols.Experimental.Testbench.Df" catch
designs that misbehave.
-}
module Tests.Protocols.Testbench.Df.Checks where

import Prelude

import Clash.Prelude qualified as C
import Data.Text (Text)
import Data.Text qualified as Text
import Protocols
import Protocols.Df qualified as Df
import Protocols.Experimental.Testbench
import Protocols.Experimental.Testbench.Df qualified as TbDf
import Test.Tasty (TestTree)
import Test.Tasty.HUnit (Assertion, assertBool, assertFailure, testCase, (@?=))
import Test.Tasty.TH (testGroupGenerator)
import Tests.Protocols.Testbench.Util

runTb :: Circuit () TraceTree -> IO TestResult
runTb = runTestbench defTestOptions{timeout = Just (cycles @Dom 1000)}

-- | Check that a testbench fails, reported by the given status, with a message containing the given text
expectFailure :: Text -> Text -> TestResult -> Assertion
expectFailure expectedSource expectedMessage r =
  case r.outcome of
    Failed{source, message} -> do
      source @?= expectedSource
      assertBool
        ("expected message containing " <> show expectedMessage <> ", got " <> show message)
        (expectedMessage `Text.isInfixOf` message)
    outcome -> assertFailure ("expected a failure, got " <> show outcome)

-- | Run a 'tb1to1' testbench with default options
run1to1 :: Dut (Df Dom Int) (Df Dom Int) -> [Int] -> [Int] -> IO TestResult
run1to1 dut xs ys = runTb (tb1to1 dut TbDf.defDriverOptions TbDf.defExpectOptions xs ys)

-- | Stall the first value for a few cycles
stallFirst :: TbDf.ExpectOptions
stallFirst =
  TbDf.defExpectOptions
    { TbDf.sinkOptions = TbDf.defSinkOptions{TbDf.stallCycles = [3]}
    }

-- | A design that ignores its input, and outputs the given values
outputs :: [Maybe Int] -> Dut (Df Dom Int) (Df Dom Int)
outputs values = comb (Circuit (\_ -> (pure (Ack False), C.fromList_lazy (values <> repeat Nothing))))

case_pass :: Assertion
case_pass = do
  r <- run1to1 (comb idC) [1 .. 5] [1 .. 5]
  r.outcome @?= Passed

case_dropsValue :: Assertion
case_dropsValue =
  run1to1 (comb (Df.filter (/= 3))) [1 .. 5] [1 .. 5]
    >>= expectFailure "tb/out/status" "output #2: expected 3, got 4"

case_duplicatesValue :: Assertion
case_duplicatesValue =
  run1to1 duplicate [1, 2] [1, 2]
    >>= expectFailure "tb/out/status" "output #1: expected 2, got 1"
 where
  duplicate = hidden (Df.expander False (\done x -> (not done, x, done)))

case_reordersValues :: Assertion
case_reordersValues =
  run1to1 (comb idC) [1, 2] [2, 1]
    >>= expectFailure "tb/out/status" "output #0: expected 2, got 1"

case_surplusValue :: Assertion
case_surplusValue =
  run1to1 (comb idC) [1, 2, 3] [1, 2]
    >>= expectFailure "tb/out/status" "output #2: unexpected output 3"

case_missingValue :: Assertion
case_missingValue = do
  r <- run1to1 (comb idC) [1, 2] [1, 2, 3]
  r.outcome @?= TimedOut{blockedBy = ["tb/out/status"]}

case_changesWhileStalled :: Assertion
case_changesWhileStalled = do
  let
    counter = C.fromList [0 :: Int ..]
    dut = comb (Circuit (\(fwd, ack) -> (ack, (\f n -> (+ n) <$> f) <$> fwd <*> counter)))
  runTb (tb1to1 dut TbDf.defDriverOptions stallFirst [1, 2] [1, 2])
    >>= expectFailure "tb/out/protocol" "data changed before it was acknowledged"

case_withdrawsData :: Assertion
case_withdrawsData =
  runTb
    ( tb1to1
        (outputs (replicate 12 Nothing <> [Just 1]))
        TbDf.defDriverOptions
        stallFirst
        []
        [1]
    )
    >>= expectFailure "tb/out/protocol" "data was withdrawn before it was acknowledged: 1"

case_undefinedData :: Assertion
case_undefinedData =
  run1to1 (outputs (repeat (Just (C.errorX "no data")))) [] [1]
    >>= expectFailure "tb/out/protocol" "data is undefined while it is offered"

case_acknowledgesDuringReset :: Assertion
case_acknowledgesDuringReset = do
  let
    dut :: Dut (Df Dom Int) (Df Dom Int)
    dut = Dut True (\_ _ _ -> Circuit (\(fwd, _) -> (pure (Ack True), fwd)))
    dOpts = TbDf.defDriverOptions{TbDf.driveDuringReset = True}
  r <- runTb (tb1to1 dut dOpts TbDf.defExpectOptions [1] [1])
  expectFailure "tb/in/status" "item #0 was acknowledged while in reset" r
  r.endTime @?= 0

case_undefinedAck :: Assertion
case_undefinedAck =
  run1to1 (comb (Circuit (\(fwd, _) -> (pure (C.errorX "no ack"), fwd)))) [1] [1]
    >>= expectFailure "tb/in/status" "acknowledgement of item #0 is undefined"

-- | Designs must not look at acknowledgements when they don't offer data
case_undefinedAckWhileIdle :: Assertion
case_undefinedAckWhileIdle = do
  let
    dOpts = TbDf.defDriverOptions{TbDf.idleCycles = [2, 0, 3]}
    eOpts =
      TbDf.defExpectOptions
        { TbDf.sinkOptions = TbDf.defSinkOptions{TbDf.idleAck = TbDf.IdleUndefined}
        }
  r <- runTb (tb1to1 (hidden (Df.registerFwd @_ @Int)) dOpts eOpts [1, 2, 3] [1, 2, 3])
  r.outcome @?= Passed

-- | Testbenches can be written using circuit notation
fifoTb :: [Int] -> Circuit () TraceTree
fifoTb values = circuit $ do
  (driverTrace, fifoIn) <- TbDf.driver "in" clk rst TbDf.defDriverOptions values
  fifoOut <- C.withClockResetEnable clk rst C.enableGen (Df.fifo C.d8) -< fifoIn
  expectTrace <- TbDf.expect "out" clk rst stallFirst values -< fifoOut
  traceGroupC "fifo" -< [driverTrace, expectTrace]
 where
  clk = C.clockGen @C.System
  rst = C.resetGenN C.d10

case_circuitNotation :: Assertion
case_circuitNotation = do
  r <- runTb (fifoTb [1 .. 20])
  r.outcome @?= Passed

tests :: TestTree
tests = $(testGroupGenerator)
