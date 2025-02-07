{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE RecordWildCards #-}

module Tests.Protocols.Wishbone where

import Clash.Hedgehog.Sized.BitVector
import Clash.Prelude hiding (not, (&&))
import Control.DeepSeq (NFData, force)
import Control.Exception (SomeException, evaluate, try)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.Either (isLeft)
import qualified Data.List as L
import GHC.Stack (HasCallStack)
import Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Protocols
import Protocols.Hedgehog (defExpectOptions)
import Protocols.Wishbone
import Protocols.Wishbone.Standard
import Protocols.Wishbone.Standard.Hedgehog
import Test.Tasty
import Test.Tasty.Hedgehog (HedgehogTestLimit (HedgehogTestLimit))
import Test.Tasty.Hedgehog.Extra (testProperty)
import Test.Tasty.TH (testGroupGenerator)
import Prelude hiding (undefined)

smallInt :: Range Int
smallInt = Range.linear 0 10

genSmallInt :: Gen Int
genSmallInt = Gen.integral smallInt

genData :: Gen a -> Gen [a]
genData = Gen.list (Range.linear 0 300)

genWishboneTransfer ::
  (KnownNat addressWidth, KnownNat (BitSize a)) =>
  Gen a ->
  Gen (WishboneMasterRequest addressWidth a)
genWishboneTransfer genA =
  Gen.choice
    [ Read <$> genDefinedBitVector <*> genDefinedBitVector
    , Write <$> genDefinedBitVector <*> genDefinedBitVector <*> genA
    ]

genWbTransferPair ::
  (KnownNat addressWidth, KnownNat (BitSize a)) =>
  Gen a ->
  Gen (WishboneMasterRequest addressWidth a, Int)
genWbTransferPair genA = liftA2 (,) (genWishboneTransfer genA) genSmallInt

-- Fourmolu only allows CPP conditions on complete top-level definitions.
blockRamUClear ::
  forall n dom a addr.
  ( HasCallStack
  , HiddenClockResetEnable dom
  , NFDataX a
  , Enum addr
  , NFDataX addr
  , 1 <= n
  ) =>
  (Index n -> a) ->
  SNat n ->
  Signal dom addr ->
  Signal dom (Maybe (addr, a)) ->
  Signal dom a
#if MIN_VERSION_clash_prelude(1,9,0)
blockRamUClear initF = blockRamU (ClearOnReset initF)
#else
blockRamUClear initF n = blockRamU ClearOnReset n initF
#endif

--
-- 'addrReadId' circuit
--

addrReadIdWb ::
  forall dom addressWidth.
  (KnownNat addressWidth) =>
  Circuit (Wishbone dom 'Standard addressWidth (BitVector addressWidth)) ()
addrReadIdWb = Circuit go
 where
  go (m2s, ()) = (reply <$> m2s, ())

  reply WishboneM2S{..}
    | busCycle && strobe && writeEnable =
        emptyWishboneS2M{acknowledge = True}
    | busCycle && strobe =
        (emptyWishboneS2M @(BitVector addressWidth))
          { acknowledge = True
          , readData = addr
          }
    | otherwise = emptyWishboneS2M

addrReadIdWbModel ::
  (KnownNat addressWidth) =>
  WishboneMasterRequest addressWidth (BitVector addressWidth) ->
  WishboneS2M (BitVector addressWidth) ->
  -- | pure state
  () ->
  Either String ()
addrReadIdWbModel (Read addr _) s@WishboneS2M{..} ()
  | acknowledge && hasX readData == Right addr = Right ()
  | otherwise =
      Left $ "Read should have been acknowledged with address as DAT " <> show s
addrReadIdWbModel Write{} s@WishboneS2M{..} ()
  | acknowledge && hasUndefined readData = Right ()
  | otherwise =
      Left $ "Write should have been acknowledged with no DAT " <> show s

prop_addrReadIdWb_model :: Property
prop_addrReadIdWb_model =
  property $
    withClockResetEnable clockGen resetGen enableGen $
      wishbonePropWithModel @System
        defExpectOptions
        addrReadIdWbModel
        addrReadIdWb
        (genData $ genWishboneTransfer @10 genDefinedBitVector)
        ()

--
-- memory element circuit
--

memoryWbModel ::
  ( KnownNat addressWidth
  , Eq a
  , ShowX a
  , NFDataX a
  , NFData a
  , Default a
  , KnownNat (BitSize a)
  ) =>
  WishboneMasterRequest addressWidth a ->
  WishboneS2M a ->
  [(BitVector addressWidth, a)] ->
  Either String [(BitVector addressWidth, a)]
memoryWbModel (Read addr sel) s st
  | sel /= maxBound && err s = Right st
  | sel /= maxBound && not (err s) =
      Left "Read with non maxBound SEL should cause ERR response"
  | otherwise = case lookup addr st of
      Just x | readData s == x && acknowledge s -> Right st
      Just x
        | otherwise ->
            Left $
              "Read from a known address did not yield the same value "
                <> showX x
                <> " : "
                <> show s
      Nothing | acknowledge s && hasX (readData s) == Right def -> Right st
      Nothing
        | otherwise ->
            Left $
              "Read from unknown address did no ACK with undefined result : "
                <> show s
memoryWbModel (Write addr sel a) s st
  | sel /= maxBound && err s = Right st
  | sel /= maxBound && not (err s) =
      Left "Write with non maxBound SEL should cause ERR response"
  | acknowledge s = Right ((addr, a) : st)
  | otherwise = Left $ "Write should be acked : " <> show s

prop_memoryWb_model :: Property
prop_memoryWb_model =
  property $
    withClockResetEnable clockGen resetGen enableGen $
      wishbonePropWithModel @System
        defExpectOptions
        memoryWbModel
        (memoryWb (blockRamUClear (const def) (SNat @256)))
        (genData (genWishboneTransfer @8 genSmallInt))
        [] -- initial state

--
-- Helpers
--

(|>>) :: Circuit () b -> Circuit b () -> (Fwd b, Bwd b)
Circuit aFn |>> Circuit bFn = (s2rAb, r2sBc)
 where
  ~((), s2rAb) = aFn ((), r2sBc)
  ~(r2sBc, ()) = bFn (s2rAb, ())

evaluateUnitCircuit ::
  (KnownDomain dom, KnownNat (BitSize a), KnownNat addressWidth, NFDataX a) =>
  Int ->
  Circuit () (Wishbone dom 'Standard addressWidth a) ->
  Circuit (Wishbone dom 'Standard addressWidth a) () ->
  Int
evaluateUnitCircuit n a b =
  let (bundle -> signals) = a |>> b
   in L.length $ sampleN n signals

--
-- validatorCircuit
--

prop_addrReadId_validator :: Property
prop_addrReadId_validator = property $ do
  reqs <- forAll $ genData (genWbTransferPair @8 genDefinedBitVector)

  let
    circuitSignalsN =
      withClockResetEnable @System clockGen resetGen enableGen $
        let
          driver = driveStandard @System @(BitVector 8) @8 defExpectOptions reqs
          addrRead = addrReadIdWb @System @8
         in
          evaluateUnitCircuit
            sampleNumber
            driver
            (validatorCircuit @System @8 @(BitVector 8) |> addrRead)

  -- force evalution of sampling somehow
  assert (circuitSignalsN == sampleNumber)

prop_memoryWb_validator :: Property
prop_memoryWb_validator = property $ do
  reqs <- forAll $ genData (genWbTransferPair @8 genDefinedBitVector)

  let
    circuitSignalsN =
      withClockResetEnable @System clockGen resetGen enableGen $
        let
          driver = driveStandard @System @(BitVector 8) @8 defExpectOptions reqs
          memory = memoryWb @System @(BitVector 8) @8 (blockRamUClear (const def) (SNat @256))
         in
          evaluateUnitCircuit
            sampleNumber
            driver
            (validatorCircuit @System @8 @(BitVector 8) |> memory)

  -- force evalution of sampling somehow
  assert (circuitSignalsN == sampleNumber)

--
-- validatorCircuitLenient
--

prop_addrReadId_validator_lenient :: Property
prop_addrReadId_validator_lenient = property $ do
  reqs <- forAll $ genData (genWbTransferPair @8 genDefinedBitVector)

  let
    circuitSignalsN =
      withClockResetEnable @System clockGen resetGen enableGen $
        let
          driver = driveStandard @System @(BitVector 8) @8 defExpectOptions reqs
          addrRead = addrReadIdWb @System @8
         in
          evaluateUnitCircuit
            sampleNumber
            driver
            (validatorCircuitLenient @System @8 @(BitVector 8) |> addrRead)

  -- force evalution of sampling somehow
  assert (circuitSignalsN == sampleNumber)

prop_memoryWb_validator_lenient :: Property
prop_memoryWb_validator_lenient = property $ do
  reqs <- forAll $ genData (genWbTransferPair @8 genDefinedBitVector)

  let
    circuitSignalsN =
      withClockResetEnable @System clockGen resetGen enableGen $
        let
          driver = driveStandard @System @(BitVector 8) @8 defExpectOptions reqs
          memory = memoryWb @System @(BitVector 8) @8 (blockRamUClear (const def) (SNat @256))
         in
          evaluateUnitCircuit
            sampleNumber
            driver
            (validatorCircuitLenient @System @8 @(BitVector 8) |> memory)

  -- force evalution of sampling somehow
  assert (circuitSignalsN == sampleNumber)

prop_specViolation_lenient :: Property
prop_specViolation_lenient = property $ do
  -- need AT LEAST one transaction so that multiple termination signals can be emitted
  reqs <- forAll $ Gen.list (Range.linear 1 500) (genWbTransferPair @8 genDefinedBitVector)

  let
    circuitSignalsN =
      withClockResetEnable @System clockGen resetGen enableGen $
        let
          driver = driveStandard @System @(BitVector 8) @8 defExpectOptions reqs
          invalid = invalidCircuit
         in
          evaluateUnitCircuit
            sampleNumber
            driver
            (validatorCircuitLenient @System @8 @(BitVector 8) |> invalid)

  -- an ErrorCall is expected, but really *any* exception is expected
  -- from the validator circuit
  res <- liftIO $ try @SomeException (evaluate (force circuitSignalsN))

  assert $ isLeft res
 where
  -- a circuit that terminates a cycle with more than one termination signal
  invalidCircuit :: Circuit (Wishbone dom 'Standard 8 (BitVector 8)) ()
  invalidCircuit = Circuit go
   where
    go (m2s, ()) = (reply <$> m2s, ())

    reply WishboneM2S{..}
      | busCycle && strobe && writeEnable =
          emptyWishboneS2M{acknowledge = True, err = True}
      | busCycle && strobe =
          (emptyWishboneS2M @(BitVector 8))
            { acknowledge = True
            , err = True
            , readData = addr
            }
      | otherwise = emptyWishboneS2M

sampleNumber :: Int
sampleNumber = 1000

tests :: TestTree
tests =
  -- TODO: Move timeout option to hedgehog for better error messages.
  -- TODO: Does not seem to work for combinatorial loops like @let x = x in x@??
  localOption (mkTimeout 60_000_000 {- 60 seconds -}) $
    localOption
      (HedgehogTestLimit (Just 1000))
      $(testGroupGenerator)
