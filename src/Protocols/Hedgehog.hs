{-|
A collection of Hedgehog helpers to test Circuit components. To test a
protocol component against a combinatorial model, see 'idWithModel'. To write
your own tester, see 'Test'.
-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE FlexibleContexts #-}

module Protocols.Hedgehog where

-- base
import Prelude
import GHC.Stack (withFrozenCallStack, HasCallStack)
import Data.Maybe (catMaybes, fromMaybe)
import Data.Kind (Type)
import Data.Proxy (Proxy(Proxy))

-- clash-protocols
import Protocols
import Protocols.Df (Df)
import Protocols.Df.Simple (Dfs)

-- clash-prelude
import qualified Clash.Prelude as C
import Clash.Prelude (type (<=), type (*))

-- data-default
import Data.Default (Default(def))

-- hedgehog
import qualified Hedgehog as H
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import qualified Hedgehog.Internal.Show as H
import qualified Hedgehog.Internal.Property as H

-- pretty-show
import Text.Show.Pretty (ppShow)

-- | Whether to stall or not. Used in 'idWithModel'.
data StallMode = NoStall | Stall
  deriving (Show, Enum, Bounded)

-- | Options for 'expect' function. See individual fields for more information.
data ExpectOptions = ExpectOptions
  { -- | Sample /n/ cycles after last expected value and check for emptiness
    eoEmptyTail :: Int

    -- | Timeout after seeing /n/ empty cycles
  , eoTimeout :: Maybe Int

    -- | Ignore first /n/ cycles
  , eoResetCycles :: Int

    -- | Start driving the circuit with its reset asserted. Circuits should
    -- never acknowledge data while this is happening.
  , eoDriveEarly :: Bool
  }

-- | Resets for 30 cycles, checks for superfluous data for 50 cycles after
-- seeing last valid data cycle, and times out after seeing 1000 consecutive
-- empty cycles.
defExpectOptions :: ExpectOptions
defExpectOptions = ExpectOptions
  { eoEmptyTail = 50
  , eoTimeout = Just 1000
  , eoResetCycles = 30
  , eoDriveEarly = True
  }

-- | Superclass class to reduce syntactical noise.
class (C.NFDataX a, C.ShowX a, C.Show a, Eq a) => TestType a
instance (C.NFDataX a, C.ShowX a, C.Show a, Eq a) => TestType a

-- | Provides a way of comparing expected data with data produced by a
-- protocol component.
class ( Simulate a
      , TestType (SimulateType a)
      , TestType (ExpectType a)

      -- Foldable requirement on Vec :(
      , 1 <= SimulateChannels a
      ) => Test a where
  type ExpectType a :: Type

  expectToSimulateType ::
    -- | Type witness
    Proxy a ->
    -- | Expect type: input for a protocol /without/ stall information
    ExpectType a ->
    -- | Expect type: input for a protocol /with/ stall information
    SimulateType a

  expect ::
    (HasCallStack, H.MonadTest m) =>
    Proxy a ->
    -- | Options, see 'ExpectOptions'
    ExpectOptions ->
    -- | Expect data
    ExpectType a ->
    -- | Actual data
    SimulateType a ->
    -- | Depending on "ExpectOptions", fails the test if:
    --
    --   * Circuit times out
    --   * Circuit produces more data than expected
    --   * Circuit produces other data than expected
    --
    m () -- TODO: Should probably return a 'Vec (SimulateChannels) Failures'
         --       in order to produce pretty reports.

instance (TestType meta, TestType a, C.KnownDomain dom) => Test (Df dom meta a) where
  type ExpectType (Df dom meta a) = [(meta, a)]

  expectToSimulateType Proxy = map Just
  expect Proxy opts expected actual =
    expect (Proxy @(Dfs dom (meta, a))) opts expected actual

instance (TestType a, C.KnownDomain dom) => Test (Dfs dom a) where
  type ExpectType (Dfs dom a) = [a]

  expectToSimulateType :: Proxy (Dfs dom a) -> [a] -> [Maybe a]
  expectToSimulateType Proxy = map Just

  expect ::
    forall m.
    (HasCallStack, H.MonadTest m) =>
    Proxy (Dfs dom a) ->
    ExpectOptions ->
    [a] ->
    [Maybe a] ->
    m ()
  expect Proxy (ExpectOptions{eoEmptyTail, eoTimeout}) expected actual = do
    go False (fromMaybe maxBound eoTimeout) expected actual
   where
    go ::
      (HasCallStack, Eq v, Show v) =>
      -- Encountered failure?
      Bool ->
      -- Timeout counter. If it reaches zero we time out.
      Int ->
      -- Expected values
      [v] ->
      -- Actual values
      [Maybe v] ->
      -- Results
      m ()
    go _failed _timeout _expected [] =
      -- This really should not happen, protocols should produce data indefinitely
      error "unexpected end of signal"
    go True _timeout [] _ =
      -- Checked all expected values, but at least one of the actual values
      -- did not match.
      failDiffWith
        "Circuit produced unexpected values"
        expected
        (take (length expected) (catMaybes actual))
    go False _timeout [] actualRest = do
      -- Check for superfluous output from protocol
      case catMaybes (take eoEmptyTail actualRest) of
        [] -> pure ()
        superfluous ->
          let err = "Circuit produced more output than expected:" in
          H.failWith Nothing (err <> "\n\n" <> ppShow superfluous)
    go failed timeout es _ | timeout <= 0 =
      if failed then
        failDiffWith
          (concat [ "Circuit did not produce enough output and produced output "
                  , "did not match the expected values." ] )
          expected
          (take (length expected - length es) (catMaybes actual))
      else
        H.failWith Nothing $ concat
          [ "Circuit did not produce enough output. Expected "
          , show (length expected), " more values:\n\n", ppShow es ]

    go failed0 timeout es (Nothing:as) = do
      -- Circuit did not output valid cycle, just continue
      go failed0 (pred timeout) es as
    go failed0 _timeout (e:es) (Just a:as) =
      -- Don't immediately stop on /= as we can still fail due to timeout
      let failed1 = e /= a || failed0 in do
      go failed1 (fromMaybe maxBound eoTimeout) es as

instance
  ( Test a
  , C.KnownNat n
  , 1 <= (n * SimulateChannels a)
  , 1 <= n ) => Test (C.Vec n a) where
  type ExpectType (C.Vec n a) = C.Vec n (ExpectType a)

  expectToSimulateType ::
    Proxy (C.Vec n a) ->
    C.Vec n (ExpectType a) ->
    C.Vec n (SimulateType a)
  expectToSimulateType Proxy =
    C.map (expectToSimulateType (Proxy @a))

  expect ::
    forall m.
    (HasCallStack, H.MonadTest m) =>
    Proxy (C.Vec n a) ->
    ExpectOptions ->
    C.Vec n (ExpectType a) ->
    C.Vec n (SimulateType a) ->
    m ()
  expect Proxy opts expecteds actuals = do
    -- TODO: This creates some pretty terrible error messages, as one
    -- TODO: simulate channel is checked at a time.
    mapM_ (uncurry (expect (Proxy @a) opts)) (C.zip expecteds actuals)

-- | Fails with an error that shows the difference between two values.
failDiffWith ::
  (H.MonadTest m, Show a, Show b, HasCallStack) =>
  -- | Additional info for error message
  String ->
  -- | Expected
  a ->
  -- | Actual
  b ->
  m ()
failDiffWith msg x y =
  case H.valueDiff <$> H.mkValue x <*> H.mkValue y of
    Nothing ->
      withFrozenCallStack $
        H.failWith Nothing $
        unlines $ [
            msg
          , "━━ expected ━━"
          , H.showPretty x
          , "━━ actual ━━"
          , H.showPretty y
          ]

    Just vdiff@(H.ValueSame _) ->
      withFrozenCallStack $
        H.failWith
          (Just $ H.Diff "━━━ Failed ("  "" "no differences" "" ") ━━━" vdiff)
          msg

    Just vdiff ->
      withFrozenCallStack $
        H.failWith
          (Just $ H.Diff "━━━ Failed (" "- expected" ") (" "+ actual" ") ━━━" vdiff)
          msg

-- | Like 'C.resetGenN', but works on 'Int' instead of 'C.SNat'. Not
-- synthesizable.
resetGen :: C.KnownDomain dom => Int -> C.Reset dom
resetGen n = C.unsafeFromHighPolarity
  (C.fromList (replicate n True <> repeat False))

-- | Test a protocol against a pure model implementation. Circuit under test will
-- be arbitrarily stalled on the left hand and right hand side and tested for
-- a number of properties:
--
--   * Whether it produces all expected data.
--   * Whether it does not produce /more/ data than expected.
--   * Whether it responds to backpressure correctly
--   * Whether it (eventually) drives a /nack/ while in reset.
--
idWithModel ::
  forall a b .
  (Test a, Test b, HasCallStack) =>
  -- | Options, see 'ExpectOptions'
  ExpectOptions ->
  -- | Test data generator, length of generated data is number of _valid_
  -- cycles. If an input consists of multiple input channels where the number
  -- of valid cycles differs, this should return the _maximum_ number of valid
  -- cycles of all channels.
  H.Gen (ExpectType a, Int) ->
  -- | Model
  (ExpectType a -> ExpectType b) ->
  -- | Implementation
  Circuit a b ->
  H.Property
idWithModel eOpts genData model prot = H.property $ do
  (dat, n) <- H.forAll genData

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
         driveC simDriveConfig (expectToSimulateType (Proxy @a) dat)
      |> lhsStallC
      |> prot
      |> rhsStallC
    sampledProtocol = sampleC simConfig stalledProtocol

  expect (Proxy @b) eOpts expected sampledProtocol

-- | Same as 'idWithModel', but with single clock, reset, and enable.
idWithModelSingleDomain ::
  forall dom a b .
  (Test a, Test b, C.KnownDomain dom, HasCallStack) =>
  -- | Options, see 'ExpectOptions'
  ExpectOptions ->
  -- | Test data generator, length of generated data is number of _valid_
  -- cycles. If an input consists of multiple input channels where the number
  -- of valid cycles differs, this should return the _maximum_ number of valid
  -- cycles of all channels.
  H.Gen (ExpectType a, Int) ->
  -- | Model
  (C.Clock dom -> C.Reset dom -> C.Enable dom -> ExpectType a -> ExpectType b) ->
  -- | Implementation
  (C.Clock dom -> C.Reset dom -> C.Enable dom -> Circuit a b) ->
  H.Property
idWithModelSingleDomain eOpts genData model0 circuit0 =
  idWithModel eOpts genData model1 circuit1
 where
  clk = C.clockGen
  rst = resetGen (eoResetCycles eOpts)
  ena = C.enableGen

  model1 = model0 clk rst ena
  circuit1 = circuit0 clk rst ena

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
