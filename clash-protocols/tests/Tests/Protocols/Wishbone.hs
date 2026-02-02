{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE RecordWildCards #-}

module Tests.Protocols.Wishbone where

import Clash.Prelude as C hiding (not, (&&))
import Control.DeepSeq (force)
import Control.Exception (SomeException, evaluate, try)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.Either (isLeft)
import Data.List qualified as L
import Data.Proxy (Proxy(..))
import Hedgehog
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range
import Protocols
import Protocols.Hedgehog (defExpectOptions, eoSampleMax)
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

genWbTransferPair ::
  (KnownNat addressBits, KnownNat dataBytes) =>
  Gen (WishboneMasterRequest addressBits dataBytes, Int)
genWbTransferPair =
  liftA2
    (,)
    (genWishboneTransfer Range.constantBounded)
    genSmallInt

--
-- 'addrReadId' circuit
--

addrReadIdWb ::
  forall dom addressBits dataBytes.
  (KnownNat addressBits, KnownNat dataBytes) =>
  Circuit (Wishbone dom 'Standard addressBits dataBytes) ()
addrReadIdWb = Circuit Proxy Proxy go
 where
  go (m2s, ()) = (reply <$> m2s, ())

  reply WishboneM2S{..}
    | busCycle && strobe && writeEnable =
        emptyWishboneS2M{acknowledge = True}
    | busCycle && strobe =
        (emptyWishboneS2M @dataBytes)
          { acknowledge = True
          , readData = resize addr
          }
    | otherwise = emptyWishboneS2M

addrReadIdWbModel ::
  (KnownNat addressBits, KnownNat dataBytes) =>
  WishboneMasterRequest addressBits dataBytes ->
  WishboneS2M dataBytes ->
  -- | pure state
  () ->
  Either String ()
addrReadIdWbModel (Read addr _) s@WishboneS2M{..} ()
  | acknowledge && hasX readData == Right (resize addr) = Right ()
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
      wishbonePropWithModel @System @10 @4
        defExpectOptions{eoSampleMax = 10_000}
        addrReadIdWbModel
        addrReadIdWb
        (genData $ genWishboneTransfer Range.constantBounded)
        ()

--
-- memory element circuit
--

memoryWbModel ::
  ( KnownNat addressBits
  , KnownNat dataBytes
  ) =>
  WishboneMasterRequest addressBits dataBytes ->
  WishboneS2M dataBytes ->
  [(BitVector addressBits, BitVector (dataBytes * 8))] ->
  Either String [(BitVector addressBits, BitVector (dataBytes * 8))]
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
      wishbonePropWithModel @System @8 @4
        defExpectOptions{eoSampleMax = 10_000}
        memoryWbModel
        (memoryWb (blockRam (C.replicate d256 0)))
        (genData (genWishboneTransfer Range.constantBounded))
        [] -- initial state

--
-- Helpers
--

(|>>) :: Circuit () b -> Circuit b () -> (Fwd b, Bwd b)
Circuit Proxy Proxy aFn |>> Circuit Proxy Proxy bFn = (s2rAb, r2sBc)
 where
  ~((), s2rAb) = aFn ((), r2sBc)
  ~(r2sBc, ()) = bFn (s2rAb, ())

evaluateUnitCircuit ::
  (KnownDomain dom, KnownNat dataBytes, KnownNat addressBits) =>
  Int ->
  Circuit () (Wishbone dom 'Standard addressBits dataBytes) ->
  Circuit (Wishbone dom 'Standard addressBits dataBytes) () ->
  Int
evaluateUnitCircuit n a b =
  let (bundle -> signals) = a |>> b
   in L.length $ sampleN n signals

--
-- validatorCircuit
--

prop_addrReadId_validator :: Property
prop_addrReadId_validator = property $ do
  reqs <- forAll $ genData (genWbTransferPair @8 @4)

  let
    circuitSignalsN =
      withClockResetEnable @System clockGen resetGen enableGen $
        let
          driver = driveStandard defExpectOptions{eoSampleMax = 10_000} reqs
          addrRead = addrReadIdWb
         in
          evaluateUnitCircuit
            sampleNumber
            driver
            (validatorCircuit |> addrRead)

  -- force evalution of sampling somehow
  assert (circuitSignalsN == sampleNumber)

prop_memoryWb_validator :: Property
prop_memoryWb_validator = property $ do
  reqs <- forAll $ genData (genWbTransferPair @8 @4)

  let
    circuitSignalsN =
      withClockResetEnable @System clockGen resetGen enableGen $
        let
          driver = driveStandard defExpectOptions{eoSampleMax = 10_000} reqs
          memory = memoryWb (blockRam (C.replicate d256 0))
         in
          evaluateUnitCircuit
            sampleNumber
            driver
            (validatorCircuit |> memory)

  -- force evalution of sampling somehow
  assert (circuitSignalsN == sampleNumber)

--
-- validatorCircuitLenient
--

prop_addrReadId_validator_lenient :: Property
prop_addrReadId_validator_lenient = property $ do
  reqs <- forAll $ genData (genWbTransferPair @8 @4)

  let
    circuitSignalsN =
      withClockResetEnable @System clockGen resetGen enableGen $
        let
          driver = driveStandard defExpectOptions{eoSampleMax = 10_000} reqs
          addrRead = addrReadIdWb
         in
          evaluateUnitCircuit
            sampleNumber
            driver
            (validatorCircuitLenient |> addrRead)

  -- force evalution of sampling somehow
  assert (circuitSignalsN == sampleNumber)

prop_memoryWb_validator_lenient :: Property
prop_memoryWb_validator_lenient = property $ do
  reqs <- forAll $ genData (genWbTransferPair @8 @4)

  let
    circuitSignalsN =
      withClockResetEnable @System clockGen resetGen enableGen $
        let
          driver = driveStandard defExpectOptions{eoSampleMax = 10_000} reqs
          memory = memoryWb (blockRam (C.replicate d256 0))
         in
          evaluateUnitCircuit
            sampleNumber
            driver
            (validatorCircuitLenient |> memory)

  -- force evalution of sampling somehow
  assert (circuitSignalsN == sampleNumber)

prop_specViolation_lenient :: Property
prop_specViolation_lenient = property $ do
  -- need AT LEAST one transaction so that multiple termination signals can be emitted
  reqs <- forAll $ Gen.list (Range.linear 1 500) (genWbTransferPair @8)

  let
    circuitSignalsN =
      withClockResetEnable @System clockGen resetGen enableGen $
        let
          driver = driveStandard @System @8 @1 defExpectOptions{eoSampleMax = 10_000} reqs
          invalid = invalidCircuit
         in
          evaluateUnitCircuit
            sampleNumber
            driver
            (validatorCircuitLenient |> invalid)

  -- an ErrorCall is expected, but really *any* exception is expected
  -- from the validator circuit
  res <- liftIO $ try @SomeException (evaluate (force circuitSignalsN))

  assert $ isLeft res
 where
  -- a circuit that terminates a cycle with more than one termination signal
  invalidCircuit :: Circuit (Wishbone dom 'Standard 8 1) ()
  invalidCircuit = Circuit Proxy Proxy go
   where
    go (m2s, ()) = (reply <$> m2s, ())

    reply WishboneM2S{..}
      | busCycle && strobe && writeEnable =
          emptyWishboneS2M{acknowledge = True, err = True}
      | busCycle && strobe =
          (emptyWishboneS2M @1)
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
