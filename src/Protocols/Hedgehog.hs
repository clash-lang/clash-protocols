{-|
A collection of Hedgehog helpers to test Circuit components. To test a
protocol component against a combinatorial model, see 'idWithModel'. To write
your own tester, see 'Test'.
-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE FlexibleContexts #-}

module Protocols.Hedgehog
 ( -- * Types
   ExpectOptions(..)
 , defExpectOptions
 , StallMode(..)
 , Test(..)
 , TestType

   -- * Test functions
 , idWithModel
 , idWithModelSingleDomain
 , propWithModel
 , propWithModelSingleDomain

   -- * Internals
 , genStallAck
 , genStallMode
 , genStalls
 ) where

-- base
import Prelude
import GHC.Stack (HasCallStack)
import Data.Proxy (Proxy(Proxy))

-- clash-protocols
import Protocols
import Protocols.Hedgehog.Internal

-- clash-prelude
import qualified Clash.Prelude as C

-- hedgehog
import qualified Hedgehog as H
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Hedgehog ((===))

-- | Whether to stall or not. Used in 'idWithModel'.
data StallMode = NoStall | Stall
  deriving (Show, Enum, Bounded)

-- | Like 'C.resetGenN', but works on 'Int' instead of 'C.SNat'. Not
-- synthesizable.
resetGen :: C.KnownDomain dom => Int -> C.Reset dom
resetGen n =
#if MIN_VERSION_clash_prelude(1,8,0)
  C.unsafeFromActiveHigh
#else
  C.unsafeFromHighPolarity
#endif
  (C.fromList (replicate n True <> repeat False))

-- | Test a protocol against a pure model implementation. Circuit under test will
-- be arbitrarily stalled on the left hand and right hand side and tested for
-- a number of properties:
--
--   * Whether it does not produce too little data.
--   * Whether it does not produce /more/ data than expected.
--   * Whether it responds to backpressure correctly
--   * Whether it (eventually) drives a /nack/ while in reset.
--
-- Finally, the data will be tested against the property supplied in the last
-- argument.
--
propWithModel ::
  forall a b .
  (Test a, Test b, HasCallStack) =>
  -- | Options, see 'ExpectOptions'
  ExpectOptions ->
  -- | Test data generator
  H.Gen (ExpectType a) ->
  -- | Model
  (ExpectType a -> ExpectType b) ->
  -- | Implementation
  Circuit a b ->
  -- | Property to test for. Function is given the data produced by the model
  -- as a first argument, and the sampled data as a second argument.
  (ExpectType b -> ExpectType b -> H.PropertyT IO ()) ->
  H.Property
propWithModel eOpts genData model prot prop = H.property $ do
  dat <- H.forAll genData
  let n = maximum (expectToLengths (Proxy @a) dat)

  -- TODO: Different distributions?
  let genStall = Gen.integral (Range.linear 0 10)

  -- Generate stalls for LHS part of the protocol. The first line determines
  -- whether to stall or not. The second determines how many cycles to stall
  -- on each _valid_ cycle.
  lhsStallModes <- H.forAll (sequenceA (C.repeat @(SimulateChannels a) genStallMode))
  lhsStalls <- H.forAll (traverse (genStalls genStall n) lhsStallModes)

  -- Generate stalls for RHS part of the protocol. The first line determines
  -- whether to stall or not. The second determines how many cycles to stall
  -- on each _valid_ cycle.
  rhsStallModes <- H.forAll (sequenceA (C.repeat @(SimulateChannels b) genStallMode))
  rhsStalls <- H.forAll (traverse (genStalls genStall n) rhsStallModes)

  let
    simConfig = def {resetCycles = eoResetCycles eOpts}
    simDriveConfig =
      if eoDriveEarly eOpts
      then def {resetCycles = max 1 (eoResetCycles eOpts - 5)}
      else def {resetCycles = eoResetCycles eOpts}
    expected = model dat
    lhsStallC = stallC simConfig lhsStalls
    rhsStallC = stallC simConfig rhsStalls
    stalledProtocol =
         driveC simDriveConfig (toSimulateType (Proxy @a) dat)
      |> lhsStallC
      |> prot
      |> rhsStallC
    sampled = sampleC simConfig stalledProtocol
    lengths = expectToLengths (Proxy @b) expected

  -- expectN errors if circuit does not produce enough data
  trimmed <- expectN (Proxy @b) eOpts lengths sampled

  _ <- H.evalNF trimmed
  _ <- H.evalNF expected

  prop expected trimmed

-- | Test a protocol against a pure model implementation. Circuit under test will
-- be arbitrarily stalled on the left hand and right hand side and tested for
-- a number of properties:
--
--   * Whether it does not produce too little data
--   * Whether it does not produce /more/ data than expected
--   * Whether the expected data corresponds to the sampled data
--   * Whether it responds to backpressure correctly
--   * Whether it (eventually) drives a /nack/ while in reset
--
-- For testing custom properties, see 'propWithModel'.
idWithModel ::
  forall a b .
  (Test a, Test b, HasCallStack) =>
  -- | Options, see 'ExpectOptions'
  ExpectOptions ->
  -- | Test data generator
  H.Gen (ExpectType a) ->
  -- | Model
  (ExpectType a -> ExpectType b) ->
  -- | Implementation
  Circuit a b ->
  H.Property
idWithModel eOpts genData model prot =
  propWithModel eOpts genData model prot (===)

-- | Same as 'propWithModel', but with single clock, reset, and enable.
propWithModelSingleDomain ::
  forall dom a b .
  (Test a, Test b, C.KnownDomain dom, HasCallStack) =>
  -- | Options, see 'ExpectOptions'
  ExpectOptions ->
  -- | Test data generator
  H.Gen (ExpectType a) ->
  -- | Model
  (C.Clock dom -> C.Reset dom -> C.Enable dom -> ExpectType a -> ExpectType b) ->
  -- | Implementation
  (C.Clock dom -> C.Reset dom -> C.Enable dom -> Circuit a b) ->
  -- | Property to test for. Function is given the data produced by the model
  -- as a first argument, and the sampled data as a second argument.
  (ExpectType b -> ExpectType b -> H.PropertyT IO ()) ->
  H.Property
propWithModelSingleDomain eOpts genData model0 circuit0 prop =
  propWithModel eOpts genData model1 circuit1 prop
 where
  clk = C.clockGen
  rst = resetGen (eoResetCycles eOpts)
  ena = C.enableGen

  model1 = model0 clk rst ena
  circuit1 = circuit0 clk rst ena

-- | Same as 'propWithModel', but with single clock, reset, and enable.
idWithModelSingleDomain ::
  forall dom a b .
  (Test a, Test b, C.KnownDomain dom, HasCallStack) =>
  -- | Options, see 'ExpectOptions'
  ExpectOptions ->
  -- | Test data generator
  H.Gen (ExpectType a) ->
  -- | Model
  (C.Clock dom -> C.Reset dom -> C.Enable dom -> ExpectType a -> ExpectType b) ->
  -- | Implementation
  (C.Clock dom -> C.Reset dom -> C.Enable dom -> Circuit a b) ->
  H.Property
idWithModelSingleDomain eOpts genData model0 circuit0 =
  propWithModelSingleDomain eOpts genData model0 circuit0 (===)

-- | Generator for 'StallMode'. Shrinks towards 'NoStall'.
genStallMode :: H.Gen StallMode
genStallMode = Gen.enumBounded

-- | Generator for 'StallMode'. Shrinks towards 'StallWithNack'.
genStallAck :: H.Gen StallAck
genStallAck = Gen.enumBounded

-- | Generator for stall information for 'stallC'. Generates stalls according
-- to distribution given in first argument. The second argument indicates how
-- many cycles the component is expecting / is producing data for. If the last
-- argument is 'NoStall', no stalls will be generated at all.
genStalls :: H.Gen Int -> Int -> StallMode -> H.Gen (StallAck, [Int])
genStalls genInt n = \case
  NoStall -> (,[]) <$> genStallAck
  Stall -> (,) <$> genStallAck <*> Gen.list (Range.singleton n) genInt
