{- |
Testbench-based testing of t'Protocols.Circuit's. This is an alternative to the
"Protocols.Experimental.Hedgehog" harness, see
<https://github.com/clash-lang/clash-protocols/issues/144>.

A testbench is a @t'Protocols.Circuit' () 'TraceTree'@ built from protocol-specific
components:

  * /drivers/ send stimuli into the design under test;
  * /sinks/ consume its output, applying backpressure;
  * /monitors/ passively check a channel against the protocol's invariants, trace
    it, record coverage, and extract its transactions;
  * /scoreboards/ and /checkers/ ("Protocols.Experimental.Testbench.Scoreboard")
    check transactions against a model;
  * /cover points/ ("Protocols.Experimental.Testbench.Coverage") record whether
    interesting situations occurred.

Every component reports a 'Status'. The runner ('runTestbench') simulates until
all statuses say 'StopOk', or one says 'Error'. It can write a waveform of
everything traced. For components for 'Protocols.Df.Df', see
"Protocols.Experimental.Testbench.Df". To use testbenches in Hedgehog
properties, see "Protocols.Experimental.Testbench.Hedgehog".

A testbench for a FIFO could look like:

@
import qualified Protocols.Experimental.Testbench.Df as TbDf

fifoTb :: [Int] -> TbDf.DriverOptions -> TbDf.ExpectOptions -> Circuit () TraceTree
fifoTb values driverOpts expectOpts = circuit $ do
  (driverTrace, fifoIn) <- TbDf.driver "in" clk rst driverOpts values
  fifoOut <- withClockResetEnable clk rst enableGen (Df.fifo d8) -< fifoIn
  expectTrace <- TbDf.expect "out" clk rst expectOpts values -< fifoOut
  traceGroupC "fifo" -< [driverTrace, expectTrace]
 where
  clk = clockGen \@System
  rst = resetGenN d10
@

And a Hedgehog property testing it:

@
prop_fifo :: Property
prop_fifo = property $ do
  values <- forAll (Gen.list (Range.linear 0 100) (Gen.int Range.linearBounded))
  driverOpts <- forAll TbDf.genDriverOptions
  expectOpts <- forAll TbDf.genExpectOptions
  assertTestbench
    defTestOptions{timeout = Just (cycles \@System 10_000)}
    [CoverGoal "fifo\/out\/stall" 10]
    (fifoTb values driverOpts expectOpts)
@
-}
module Protocols.Experimental.Testbench (
  module Protocols.Experimental.Testbench.Trace,
  module Protocols.Experimental.Testbench.Run,
  module Protocols.Experimental.Testbench.Coverage,
) where

import Protocols.Experimental.Testbench.Coverage
import Protocols.Experimental.Testbench.Run
import Protocols.Experimental.Testbench.Trace
