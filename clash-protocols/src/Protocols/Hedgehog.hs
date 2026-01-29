{-# LANGUAGE UndecidableInstances #-}

{- |
A collection of Hedgehog helpers to test Circuit components. To test a
protocol component against a combinatorial model, see 'idWithModel'. To write
your own tester, see 'Test'.
-}
module Protocols.Hedgehog (
  -- * Types
  ExpectOptions (..),
  defExpectOptions,
  StallMode (..),
  Test (..),
  TestType,

  -- * Test functions
  idWithModel,
  idWithModelSingleDomain,
  propWithModel,
  propWithModelSingleDomain,

  -- * Monadic test functions
  idWithModelT,
  idWithModelSingleDomainT,
  propWithModelT,
  propWithModelSingleDomainT,

  -- * Internals
  genStallAck,
  genStallMode,
  genStalls,
  expectedEmptyCycles,
) where

-- base
import Control.Concurrent (threadDelay)
import Control.Monad (when)
import Control.Monad.IO.Class (liftIO, MonadIO)
import Data.Proxy (Proxy (Proxy))
import GHC.Stack (HasCallStack)
import Prelude

-- clash-protocols
import Protocols
import Protocols.Hedgehog.Internal

-- clash-prelude
import Clash.Prelude qualified as C

-- clash-prelude-hedgehog
import Clash.Hedgehog.Sized.Vector (genVec)

-- hedgehog
import Hedgehog ((===))
import Hedgehog qualified as H
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range

-- lifted-async
import Control.Concurrent.Async.Lifted

-- monad-control
import Control.Monad.Trans.Control (MonadBaseControl)

-- | Whether to stall or not. Used in 'idWithModel'.
data StallMode = NoStall | Stall
  deriving (Show, Enum, Bounded)

{- | Like 'C.resetGenN', but works on 'Int' instead of 'C.SNat'. Not
synthesizable.
-}
resetGen :: (C.KnownDomain dom) => Int -> C.Reset dom
resetGen n =
  C.unsafeFromActiveHigh
    (C.fromList (replicate n True <> repeat False))

{- | Attach a timeout to a property. Fails if the property does not finish in
the given time. The timeout is given in milliseconds.
-}
withTimeoutMs :: (MonadIO m, MonadBaseControl IO m) => Int -> H.PropertyT m a -> H.PropertyT m a
withTimeoutMs timeout v = do
  result <-
    race
      (liftIO $ threadDelay (timeout * 1000))
      v
  case result of
    Left () -> fail "Timeout exceeded"
    Right x -> pure x

{- | Test a protocol against a pure model implementation. Circuit under test will
be arbitrarily stalled on the left hand and right hand side and tested for
a number of properties:

  * Whether it does not produce too little data.
  * Whether it does not produce /more/ data than expected.
  * Whether it responds to backpressure correctly
  * Whether it (eventually) drives a /nack/ while in reset.

Finally, the data will be tested against the property supplied in the last
argument.
-}
propWithModel ::
  forall a b.
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
propWithModel eOps gen model dut prop = H.property $ propWithModelT eOps gen model dut prop

{- |
Monadic version of 'propWithModel'.
Allows property-based protocol testing in any monad supporting 'MonadIO' and 'MonadBaseControl IO'.
This is useful for integrating with monadic test frameworks or when additional effects are needed during testing.
-}
propWithModelT ::
  forall a b m .
  (Test a, Test b, HasCallStack, Monad m, MonadIO m, MonadBaseControl IO m) =>
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
  (ExpectType b -> ExpectType b -> H.PropertyT m ()) ->
  H.PropertyT m ()
propWithModelT eOpts genData model prot prop =
  maybe id withTimeoutMs (eoTimeoutMs eOpts) $ do
    dat <- H.forAll genData
    when (eoTrace eOpts) $ liftIO $ putStr "propWithModel: dat: " >> print dat

    -- TODO: Different 'n's for each output
    n <- H.forAll (Gen.integral (Range.linear 0 (eoStallsMax eOpts)))
    when (eoTrace eOpts) $ liftIO $ putStr "propWithModel: n: " >> print n

    -- TODO: Different distributions?
    let
      genStall = Gen.int (Range.linear 1 eOpts.eoConsecutiveStalls)

    -- Generate stalls for LHS part of the protocol. The first line determines
    -- whether to stall or not. The second determines how many cycles to stall
    -- on each _valid_ cycle.
    lhsStallModes <- H.forAll (genVec genStallMode)
    when (eoTrace eOpts) $
      liftIO $
        putStr "propWithModel: lhsStallModes: " >> print lhsStallModes
    lhsStalls <- H.forAll (traverse (genStalls genStall n) lhsStallModes)
    when (eoTrace eOpts) $ liftIO $ putStr "propWithModel: lhsStalls: " >> print lhsStalls

    -- Generate stalls for RHS part of the protocol. The first line determines
    -- whether to stall or not. The second determines how many cycles to stall
    -- on each _valid_ cycle.
    rhsStallModes <- H.forAll (genVec genStallMode)
    when (eoTrace eOpts) $
      liftIO $
        putStr "propWithModel: rhsStallModes: " >> print rhsStallModes
    rhsStalls <- H.forAll (traverse (genStalls genStall n) rhsStallModes)
    when (eoTrace eOpts) $ liftIO $ putStr "propWithModel: rhsStalls: " >> print rhsStalls

    let
      simConfig = def{resetCycles = eoResetCycles eOpts}
      simDriveConfig =
        if eoDriveEarly eOpts
          then def{resetCycles = max 1 (eoResetCycles eOpts - 5)}
          else def{resetCycles = eoResetCycles eOpts}
      expected = model dat
      lhsStallC = stallC simConfig lhsStalls
      rhsStallC = stallC simConfig rhsStalls
      stalledProtocol =
        driveC simDriveConfig (toSimulateType (Proxy @a) dat)
          |> lhsStallC
          |> prot
          |> rhsStallC
      sampled = sampleC simConfig stalledProtocol

    -- expectN errors if circuit does not produce enough data
    trimmed <- expectN (Proxy @b) eOpts sampled

    when (eoTrace eOpts) $ liftIO $ putStrLn "propWithModel: before forcing trimmed.."
    _ <- H.evalNF trimmed
    when (eoTrace eOpts) $ liftIO $ putStrLn "propWithModel: before forcing expected.."
    _ <- H.evalNF expected

    when (eoTrace eOpts) $
      liftIO $
        putStrLn "propWithModel: executing property.."
    prop expected trimmed

{- | Test a protocol against a pure model implementation. Circuit under test will
be arbitrarily stalled on the left hand and right hand side and tested for
a number of properties:

  * Whether it does not produce too little data
  * Whether it does not produce /more/ data than expected
  * Whether the expected data corresponds to the sampled data
  * Whether it responds to backpressure correctly
  * Whether it (eventually) drives a /nack/ while in reset

For testing custom properties, see 'propWithModel'.
-}
idWithModel ::
  forall a b.
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
idWithModel eOpts genData model prot = H.property $ idWithModelT eOpts genData model prot

{- |
Monadic version of 'idWithModel'.
Runs the protocol-vs-model test using the default equality property (===) in any monad supporting 'MonadIO' and 'MonadBaseControl IO'.
Use this when you want to run the test in a monadic context or need additional effects.
-}
idWithModelT ::
  forall a b m.
  (Test a, Test b, HasCallStack, Monad m, MonadIO m, MonadBaseControl IO m) =>
  -- | Options, see 'ExpectOptions'
  ExpectOptions ->
  -- | Test data generator
  H.Gen (ExpectType a) ->
  -- | Model
  (ExpectType a -> ExpectType b) ->
  -- | Implementation
  Circuit a b ->
  H.PropertyT m ()
idWithModelT eOpts genData model prot = propWithModelT eOpts genData model prot (===)

-- | Same as 'propWithModel', but with single clock, reset, and enable.
propWithModelSingleDomain ::
  forall dom a b.
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
propWithModelSingleDomain eOpts genData model dut prop = H.property $
  propWithModelSingleDomainT eOpts genData model dut prop

{- |
Monadic version of 'propWithModelSingleDomain'.
Allows property-based protocol testing for single-domain circuits in any monad supporting 'MonadIO' and 'MonadBaseControl IO'.
This is useful for monadic test integration or when effects are required during testing.
-}
propWithModelSingleDomainT ::
  forall dom a b m.
  (Test a, Test b, C.KnownDomain dom, HasCallStack, Monad m, MonadIO m, MonadBaseControl IO m) =>
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
  (ExpectType b -> ExpectType b -> H.PropertyT m ()) ->
  H.PropertyT m ()
propWithModelSingleDomainT eOpts genData model0 circuit0 =
  propWithModelT eOpts genData model1 circuit1
 where
  clk = C.clockGen
  rst = resetGen (eoResetCycles eOpts)
  ena = C.enableGen

  model1 = model0 clk rst ena
  circuit1 = circuit0 clk rst ena

-- | Same as 'propWithModel', but with single clock, reset, and enable.
idWithModelSingleDomain ::
  forall dom a b.
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
idWithModelSingleDomain eOpts genData model0 circuit0 = H.property
  $ idWithModelSingleDomainT eOpts genData model0 circuit0

{- |
Monadic version of 'idWithModelSingleDomain'.
Runs the single-domain protocol-vs-model test using the default equality property (===) in any monad supporting 'MonadIO' and 'MonadBaseControl IO'.
Use this for monadic test frameworks or when additional effects are needed.
-}
idWithModelSingleDomainT ::
  forall dom a b m.
  (Test a, Test b, C.KnownDomain dom, HasCallStack, Monad m, MonadIO m, MonadBaseControl IO m) =>
  -- | Options, see 'ExpectOptions'
  ExpectOptions ->
  -- | Test data generator
  H.Gen (ExpectType a) ->
  -- | Model
  (C.Clock dom -> C.Reset dom -> C.Enable dom -> ExpectType a -> ExpectType b) ->
  -- | Implementation
  (C.Clock dom -> C.Reset dom -> C.Enable dom -> Circuit a b) ->
  H.PropertyT m ()
idWithModelSingleDomainT eOpts genData model0 circuit0 =
  propWithModelSingleDomainT eOpts genData model0 circuit0 (===)

-- | Generator for 'StallMode'. Shrinks towards 'NoStall'.
genStallMode :: H.Gen StallMode
genStallMode = Gen.enumBounded

-- | Generator for 'StallMode'. Shrinks towards 'StallWithNack'.
genStallAck :: H.Gen StallAck
genStallAck = Gen.enumBounded

{- | Generator for stall information for 'stallC'. Generates stalls according
to distribution given in first argument. The second argument indicates how
many cycles the component is expecting / is producing data for. If the last
argument is 'NoStall', no stalls will be generated at all.
-}
genStalls :: H.Gen Int -> Int -> StallMode -> H.Gen (StallAck, [Int])
genStalls genInt n = \case
  NoStall -> (,[]) <$> genStallAck
  Stall -> (,) <$> genStallAck <*> Gen.list (Range.singleton n) genInt
