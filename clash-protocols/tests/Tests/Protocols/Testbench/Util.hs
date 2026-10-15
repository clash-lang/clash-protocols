{-# LANGUAGE NoImplicitPrelude #-}

-- | Helpers to build testbenches for 'Df' designs in a few common shapes
module Tests.Protocols.Testbench.Util where

import Clash.Prelude hiding (traceSignal)

import Clash.Hedgehog.Sized.Vector (genVec)
import Control.Monad.IO.Class (MonadIO, liftIO)
import GHC.Stack (HasCallStack, withFrozenCallStack)
import Hedgehog (Gen, MonadTest, Property, forAll, property)
import Hedgehog.Internal.Property (CoverPercentage)
import Protocols (Ack, Bwd, Circuit (..), Df, Fwd)
import Protocols.Experimental.Testbench
import Protocols.Experimental.Testbench.Hedgehog (assertTestbench)
import System.Directory (createDirectoryIfMissing, getTemporaryDirectory)
import System.FilePath ((</>))

import Prelude qualified as P

import Data.Text qualified as Text

import Protocols.Experimental.Testbench.Df qualified as TbDf

type Dom = System

tbClock :: Clock Dom
tbClock = clockGen

tbReset :: Reset Dom
tbReset = resetGenN (SNat @10)

tbEnable :: Enable Dom
tbEnable = enableGen

-- | A design under test
data Dut a b = Dut
  { dutHasReset :: Bool
  {- ^ Whether the design has a reset. Designs without a reset may acknowledge
  data during the testbench's reset, so drivers don't drive during reset.
  -}
  , dutCircuit :: Clock Dom -> Reset Dom -> Enable Dom -> Circuit a b
  }

-- | A design with a hidden clock, reset, and enable
hidden :: ((HiddenClockResetEnable Dom) => Circuit a b) -> Dut a b
hidden dut = Dut True (\clk rst ena -> withClockResetEnable clk rst ena dut)

-- | A combinational design
comb :: Circuit a b -> Dut a b
comb dut = Dut False (\_ _ _ -> dut)

-- | Instantiate a design with the testbench's clock, reset, and enable
instantiate :: Dut a b -> Circuit a b
instantiate dut = dutCircuit dut tbClock tbReset tbEnable

-- | Don't drive during reset if the design has no reset, see 'dutHasReset'
duringReset :: Dut a b -> TbDf.DriverOptions -> TbDf.DriverOptions
duringReset dut opts = opts{TbDf.driveDuringReset = opts.driveDuringReset && dutHasReset dut}

runC :: Circuit a b -> Fwd a -> Bwd b -> (Bwd a, Fwd b)
runC (Circuit f) a b = f (a, b)

-- | Wrap traces in a group called @tb@, together with the clock and reset
testbench :: [TraceTree] -> Circuit () TraceTree
testbench traces =
  Circuit $ \_ ->
    ((), traceGroup "tb" (traceClock "clk" tbClock : traceReset "rst" tbReset : traces))

{- | Run a testbench in Hedgehog. Writes a waveform to the temporary directory if
it fails.
-}
assertTb ::
  (MonadIO m, MonadTest m, HasCallStack) =>
  String -> [CoverGoal] -> Circuit () TraceTree -> m ()
assertTb name goals tb = withFrozenCallStack $ do
  dir <- liftIO $ do
    tmp <- getTemporaryDirectory
    let d = tmp </> "clash-protocols-testbench"
    createDirectoryIfMissing True d
    P.pure d
  assertTestbench
    TestOptions
      { vcdOutput = VcdOnFailure (dir </> name P.<> ".vcd")
      , timeout = P.Just (cycles @Dom 100_000)
      }
    goals
    tb

-- | A coverage goal for a cover point in the @tb@ group
goal :: String -> CoverPercentage -> CoverGoal
goal path = CoverGoal (Text.pack ("tb/" P.<> path))

{- | Coverage goals for the input and output channels of a design under test:
both should stall regularly, and the output should transfer data back to back.
-}
dfGoals :: String -> String -> [CoverGoal]
dfGoals input output = stallGoals input output P.<> [goal (output P.<> "/back_to_back") 5]

{- | Like 'dfGoals', for designs that cannot produce output in consecutive cycles.
Pass an empty input name for designs that never stall their input.
-}
stallGoals :: String -> String -> [CoverGoal]
stallGoals input output =
  [goal (input P.<> "/stall") 5 | input P./= ""] P.<> [goal (output P.<> "/stall") 5]

-- | A driver, sending into the design under test, and an expecter, checking its output
tb1to1 ::
  (BitPack a, ShowX a, BitPack b, ShowX b, Eq b) =>
  Dut (Df Dom a) (Df Dom b) ->
  TbDf.DriverOptions ->
  TbDf.ExpectOptions ->
  [a] ->
  [b] ->
  Circuit () TraceTree
tb1to1 dut dOpts eOpts xs ys = testbench [drvT, expT]
 where
  (_, (drvT, a)) = runC (TbDf.driver "in" tbClock tbReset (duringReset dut dOpts) xs) () ((), aAck)
  (aAck, b) = runC (instantiate dut) a bAck
  (bAck, expT) = runC (TbDf.expect "out" tbClock tbReset eOpts ys) b ()

-- | Like 'tb1to1', for designs with multiple outputs
tb1toN ::
  (KnownNat n, BitPack a, ShowX a, BitPack b, ShowX b, Eq b) =>
  Dut (Df Dom a) (Vec n (Df Dom b)) ->
  TbDf.DriverOptions ->
  Vec n TbDf.ExpectOptions ->
  [a] ->
  Vec n [b] ->
  Circuit () TraceTree
tb1toN dut dOpts eOpts xs yss = testbench (drvT : toList (snd <$> expects))
 where
  (_, (drvT, a)) = runC (TbDf.driver "in" tbClock tbReset (duringReset dut dOpts) xs) () ((), aAck)
  (aAck, bs) = runC (instantiate dut) a (fst <$> expects)
  expects = expect1 <$> indicesI <*> eOpts <*> yss <*> lazyV bs
  expect1 i o ys b = runC (TbDf.expect ("out" P.<> show i) tbClock tbReset o ys) b ()

-- | Like 'tb1to1', for designs with two outputs
tb1to2 ::
  (BitPack a, ShowX a, BitPack b, ShowX b, Eq b, BitPack c, ShowX c, Eq c) =>
  Dut (Df Dom a) (Df Dom b, Df Dom c) ->
  TbDf.DriverOptions ->
  (TbDf.ExpectOptions, TbDf.ExpectOptions) ->
  [a] ->
  ([b], [c]) ->
  Circuit () TraceTree
tb1to2 dut dOpts (eOpts0, eOpts1) xs (ys, zs) = testbench [drvT, expT0, expT1]
 where
  (_, (drvT, a)) = runC (TbDf.driver "in" tbClock tbReset (duringReset dut dOpts) xs) () ((), aAck)
  (aAck, (b, c)) = runC (instantiate dut) a (bAck, cAck)
  (bAck, expT0) = runC (TbDf.expect "out0" tbClock tbReset eOpts0 ys) b ()
  (cAck, expT1) = runC (TbDf.expect "out1" tbClock tbReset eOpts1 zs) c ()

-- | Drivers for every input of a design under test
drivers ::
  (KnownNat n, BitPack a, ShowX a) =>
  String ->
  Vec n TbDf.DriverOptions ->
  Vec n [a] ->
  Vec n (Signal Dom Ack) ->
  (Vec n TraceTree, Vec n (Signal Dom (Maybe a)))
drivers prefix dOpts xss acks = (fst <$> results, snd <$> results)
 where
  results = driver1 <$> indicesI <*> dOpts <*> xss <*> lazyV acks
  driver1 i o xs ack =
    snd (runC (TbDf.driver (prefix P.<> show i) tbClock tbReset o xs) () ((), ack))

-- | Drivers for every input of a design under test, and an expecter on its output
tbNto1 ::
  (KnownNat n, BitPack a, ShowX a, BitPack b, ShowX b, Eq b) =>
  Dut (Vec n (Df Dom a)) (Df Dom b) ->
  Vec n TbDf.DriverOptions ->
  TbDf.ExpectOptions ->
  Vec n [a] ->
  [b] ->
  Circuit () TraceTree
tbNto1 dut dOpts eOpts xss ys = testbench (toList drvTs P.<> [expT])
 where
  (drvTs, as) = drivers "in" (duringReset dut <$> dOpts) xss aAcks
  (aAcks, b) = runC (instantiate dut) as bAck
  (bAck, expT) = runC (TbDf.expect "out" tbClock tbReset eOpts ys) b ()

-- | Like 'tbNto1', for designs with two inputs
tb2to1 ::
  (BitPack a, ShowX a, BitPack b, ShowX b, BitPack c, ShowX c, Eq c) =>
  Dut (Df Dom a, Df Dom b) (Df Dom c) ->
  (TbDf.DriverOptions, TbDf.DriverOptions) ->
  TbDf.ExpectOptions ->
  ([a], [b]) ->
  [c] ->
  Circuit () TraceTree
tb2to1 dut (dOpts0, dOpts1) eOpts (xs0, xs1) ys = testbench [drvT0, drvT1, expT]
 where
  (_, (drvT0, a0)) = runC (TbDf.driver "in0" tbClock tbReset (duringReset dut dOpts0) xs0) () ((), aAck0)
  (_, (drvT1, a1)) = runC (TbDf.driver "in1" tbClock tbReset (duringReset dut dOpts1) xs1) () ((), aAck1)
  ((aAck0, aAck1), b) = runC (instantiate dut) (a0, a1) bAck
  (bAck, expT) = runC (TbDf.expect "out" tbClock tbReset eOpts ys) b ()

-- | Drivers for a vector of data inputs and a selection input
tbSelect ::
  (KnownNat n, BitPack a, ShowX a, BitPack s, ShowX s, BitPack b, ShowX b, Eq b) =>
  Dut (Vec n (Df Dom a), Df Dom s) (Df Dom b) ->
  Vec n TbDf.DriverOptions ->
  TbDf.DriverOptions ->
  TbDf.ExpectOptions ->
  Vec n [a] ->
  [s] ->
  [b] ->
  Circuit () TraceTree
tbSelect dut dOpts sOpts eOpts xss ss ys = testbench (toList drvTs P.<> [selT, expT])
 where
  (drvTs, as) = drivers "in" (duringReset dut <$> dOpts) xss aAcks
  (_, (selT, s)) = runC (TbDf.driver "select" tbClock tbReset (duringReset dut sOpts) ss) () ((), sAck)
  ((aAcks, sAck), b) = runC (instantiate dut) (as, s) bAck
  (bAck, expT) = runC (TbDf.expect "out" tbClock tbReset eOpts ys) b ()

-- | Property for 'tb1to1'
prop1to1 ::
  (Show a, BitPack a, ShowX a, BitPack b, ShowX b, Eq b) =>
  String ->
  [CoverGoal] ->
  Gen [a] ->
  ([a] -> [b]) ->
  Dut (Df Dom a) (Df Dom b) ->
  Property
prop1to1 name goals genXs model dut = property $ do
  xs <- forAll genXs
  dOpts <- forAll TbDf.genDriverOptions
  eOpts <- forAll TbDf.genExpectOptions
  assertTb name goals (tb1to1 dut dOpts eOpts xs (model xs))

-- | Property for 'tb1toN'
prop1toN ::
  (KnownNat n, 1 <= n, Show a, BitPack a, ShowX a, BitPack b, ShowX b, Eq b) =>
  String ->
  [CoverGoal] ->
  Gen [a] ->
  ([a] -> Vec n [b]) ->
  Dut (Df Dom a) (Vec n (Df Dom b)) ->
  Property
prop1toN name goals genXs model dut = property $ do
  xs <- forAll genXs
  dOpts <- forAll TbDf.genDriverOptions
  eOpts <- forAll (genVec TbDf.genExpectOptions)
  assertTb name goals (tb1toN dut dOpts eOpts xs (model xs))

-- | Property for 'tb1to2'
prop1to2 ::
  (Show a, BitPack a, ShowX a, BitPack b, ShowX b, Eq b, BitPack c, ShowX c, Eq c) =>
  String ->
  [CoverGoal] ->
  Gen [a] ->
  ([a] -> ([b], [c])) ->
  Dut (Df Dom a) (Df Dom b, Df Dom c) ->
  Property
prop1to2 name goals genXs model dut = property $ do
  xs <- forAll genXs
  dOpts <- forAll TbDf.genDriverOptions
  eOpts0 <- forAll TbDf.genExpectOptions
  eOpts1 <- forAll TbDf.genExpectOptions
  assertTb name goals (tb1to2 dut dOpts (eOpts0, eOpts1) xs (model xs))

-- | Property for 'tbNto1'
propNto1 ::
  (KnownNat n, 1 <= n, Show a, BitPack a, ShowX a, BitPack b, ShowX b, Eq b) =>
  String ->
  [CoverGoal] ->
  Gen (Vec n [a]) ->
  (Vec n [a] -> [b]) ->
  Dut (Vec n (Df Dom a)) (Df Dom b) ->
  Property
propNto1 name goals genXss model dut = property $ do
  xss <- forAll genXss
  dOpts <- forAll (genVec TbDf.genDriverOptions)
  eOpts <- forAll TbDf.genExpectOptions
  assertTb name goals (tbNto1 dut dOpts eOpts xss (model xss))

-- | Property for 'tb2to1'
prop2to1 ::
  (Show a, BitPack a, ShowX a, Show b, BitPack b, ShowX b, BitPack c, ShowX c, Eq c) =>
  String ->
  [CoverGoal] ->
  Gen ([a], [b]) ->
  (([a], [b]) -> [c]) ->
  Dut (Df Dom a, Df Dom b) (Df Dom c) ->
  Property
prop2to1 name goals genXs model dut = property $ do
  xs <- forAll genXs
  dOpts0 <- forAll TbDf.genDriverOptions
  dOpts1 <- forAll TbDf.genDriverOptions
  eOpts <- forAll TbDf.genExpectOptions
  assertTb name goals (tb2to1 dut (dOpts0, dOpts1) eOpts xs (model xs))

-- | Property for 'tbSelect'
propSelect ::
  ( KnownNat n
  , 1 <= n
  , Show a
  , BitPack a
  , ShowX a
  , Show s
  , BitPack s
  , ShowX s
  , BitPack b
  , ShowX b
  , Eq b
  ) =>
  String ->
  [CoverGoal] ->
  Gen (Vec n [a], [s]) ->
  ((Vec n [a], [s]) -> [b]) ->
  Dut (Vec n (Df Dom a), Df Dom s) (Df Dom b) ->
  Property
propSelect name goals genInputs model dut = property $ do
  (xss, ss) <- forAll genInputs
  dOpts <- forAll (genVec TbDf.genDriverOptions)
  sOpts <- forAll TbDf.genDriverOptions
  eOpts <- forAll TbDf.genExpectOptions
  assertTb name goals (tbSelect dut dOpts sOpts eOpts xss ss (model (xss, ss)))
