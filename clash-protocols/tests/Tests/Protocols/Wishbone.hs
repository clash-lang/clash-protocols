{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fconstraint-solver-iterations=10 #-}
{-# OPTIONS_GHC -fplugin=Protocols.Plugin #-}

module Tests.Protocols.Wishbone where

import Clash.Prelude as C

import Clash.Hedgehog.Sized.Vector (genVec)
import Clash.Sized.Vector.ToTuple (vecToTuple)
import Control.DeepSeq (force)
import Control.Exception (SomeException, evaluate, try)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.Either (isLeft)
import Data.List qualified as L
import Data.String.Interpolate (i)
import Hedgehog
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range
import Protocols
import Protocols.Experimental.Hedgehog (ExpectOptions (..), defExpectOptions)
import Protocols.Experimental.Simulate
import Protocols.Experimental.Wishbone
import Protocols.Experimental.Wishbone.Standard
import Protocols.Experimental.Wishbone.Standard.Hedgehog
import Protocols.Experimental.Wishbone.Standard.Hedgehog qualified as WbStd
import Protocols.Internal (circuitMonitor)
import Test.Tasty
import Test.Tasty.Hedgehog (HedgehogTestLimit (HedgehogTestLimit))
import Test.Tasty.Hedgehog.Extra (testProperty)
import Test.Tasty.TH (testGroupGenerator)

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
addrReadIdWb = Circuit go
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
  property
    $ withClockResetEnable clockGen resetGen enableGen
    $ wishbonePropWithModel @System @10 @4
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
            Left
              $ "Read from a known address did not yield the same value "
              <> showX x
              <> " : "
              <> show s
      Nothing | acknowledge s && hasX (readData s) == Right def -> Right st
      Nothing
        | otherwise ->
            Left
              $ "Read from unknown address did no ACK with undefined result : "
              <> show s
memoryWbModel (Write addr sel a) s st
  | sel /= maxBound && err s = Right st
  | sel /= maxBound && not (err s) =
      Left "Write with non maxBound SEL should cause ERR response"
  | acknowledge s = Right ((addr, a) : st)
  | otherwise = Left $ "Write should be acked : " <> show s

prop_memoryWb_model :: Property
prop_memoryWb_model =
  property
    $ withClockResetEnable clockGen resetGen enableGen
    $ wishbonePropWithModel @System @8 @4
      defExpectOptions{eoSampleMax = 10_000}
      memoryWbModel
      (memoryWb (blockRam (C.replicate d256 0)))
      (genData (genWishboneTransfer Range.constantBounded))
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
      withClockResetEnable @System clockGen resetGen enableGen
        $ let
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
      withClockResetEnable @System clockGen resetGen enableGen
        $ let
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
      withClockResetEnable @System clockGen resetGen enableGen
        $ let
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
      withClockResetEnable @System clockGen resetGen enableGen
        $ let
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
      withClockResetEnable @System clockGen resetGen enableGen
        $ let
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
  invalidCircuit = Circuit go
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

--
-- latchResponse
--

-- | Check whether 'latchResponse' acts as an identity circuit on a transactional level.
prop_latchResponse_identity :: Property
prop_latchResponse_identity = property $ do
  reqs <- forAll $ genData (genWishboneTransfer @8 @4 Range.linearBounded)
  earlyBusCycle <- forAll Gen.bool
  driveStalls <- forAll $ Gen.list (Range.singleton (L.length reqs)) genSmallInt
  memStalls <- forAll $ Gen.list (Range.singleton (L.length reqs)) genSmallInt
  let
    eOpts = defExpectOptions{eoSampleMax = 10_000}

    run ::
      ((HiddenClockResetEnable System) => Circuit (Wishbone System 'Standard 8 4) ()) ->
      [WishboneS2M 4]
    run ckt =
      withClockResetEnable @System clockGen resetGen enableGen
        $ L.filter hasTerminateFlag
        $ fmap snd
        $ WbStd.sample
          eOpts
          (driveStandard eOpts (L.zip reqs driveStalls))
          ckt

    expected = run (memoryWb (blockRam (C.replicate d256 0)))
    actual =
      run
        ( latchResponse
            |> stallStandard earlyBusCycle memStalls
            |> memoryWb (blockRam (C.replicate d256 0))
        )

  L.length actual === L.length expected

  footnote [i| Expected: #{expected}|]
  footnote [i| Actual: #{actual}|]
  assert (and $ L.zipWith3 eqWishboneS2M reqs actual expected)

-- | Check whether 'latchResponse' retains the last response until a new transaction is started.
prop_latchResponse_retains :: Property
prop_latchResponse_retains = property $ do
  reqs <-
    forAll $ Gen.list (Range.linear 1 300) (genWishboneTransfer @8 @4 Range.linearBounded)
  earlyBusCycle <- forAll Gen.bool
  driveStalls <- forAll $ Gen.list (Range.singleton (L.length reqs)) genSmallInt
  memStalls <- forAll $ Gen.list (Range.singleton (L.length reqs)) genSmallInt
  let
    eOpts = defExpectOptions{eoResetCycles = 5}

    trace =
      withClockResetEnable @System clockGen resetGen enableGen
        $ sampleUnfiltered
          eOpts
          (driveStandard eOpts (L.zip reqs driveStalls))
          ( latchResponse
              |> stallStandard earlyBusCycle memStalls
              |> memoryWb (blockRam (C.replicate d256 0))
          )

    checkCycle (lastReq, latched) (m2s, s2m)
      | busCycle m2s =
          ((Just m2s, s2m), True)
      | Just req <- lastReq =
          ((lastReq, latched), eqWishboneS2M (m2sToRequest req) latched s2m)
      | otherwise =
          ((lastReq, latched), True)

    (_, checks) = L.mapAccumL checkCycle (Nothing, emptyWishboneS2M) trace

  footnoteShow trace
  assert $ and checks

--
-- splitOnMask
--

{- | We test splitOnMask by checking if we can use it to create a byte addresable version of 'memoryWb'.
To do so we generate arbitrary wishbone transactions, split those into separate lists of transactions
(one for each byte lane) and supply those to single byte wide versions of 'memoryWb'.
We combine the responses we obtain and that should behave the same as driveStandard |> splitOnMask |> repeatC (memoryWb (blockRam (C.replicate d256 0))).
-}
prop_splitOnMask :: Property
prop_splitOnMask = property $ do
  reqs <- forAll $ genData (genWishboneTransfer @8 @4 Range.constantBounded)
  stallCounts <- forAll $ Gen.list (Range.singleton (L.length reqs)) genSmallInt
  stallsLanes <- forAll $ genVec @4 $ Gen.list (Range.singleton (L.length reqs)) genSmallInt
  let
    reqPairs = L.zip reqs stallCounts
    eOpts = defExpectOptions{eoSampleMax = 10_000}

    splitReq :: WishboneMasterRequest 8 4 -> Vec 4 (WishboneMasterRequest 8 1)
    splitReq (Read addr sel) =
      C.map (Read addr . resize . (sel `shiftR`) . fromIntegral) indicesI
    splitReq (Write addr sel dat) =
      C.map
        ( (\idx -> Write addr (resize $ shiftR sel idx) (resize $ shiftR dat (idx * 8)))
            . fromIntegral
        )
        indicesI

    runLane :: [WishboneMasterRequest 8 1] -> [Int] -> [WishboneS2M 1]
    runLane laneReqs laneStalls =
      withClockResetEnable @System clockGen resetGen enableGen
        $ L.filter hasTerminateFlag
        $ fmap snd
        $ WbStd.sample
          eOpts
          (driveStandard eOpts (L.zip laneReqs laneStalls))
          (memoryWb (blockRam (C.replicate d256 0)))

    perLaneReqs = sequenceA $ L.map splitReq reqs

    combine lanes =
      (emptyWishboneS2M @4)
        { readData = pack $ fmap (.readData) lanes
        , acknowledge = C.all (.acknowledge) lanes
        , err = C.any (.err) lanes
        }

    (l0, l1, l2, l3) = vecToTuple $ reverse $ zipWith runLane perLaneReqs stallsLanes
    expected = L.zipWith4 (\s0 s1 s2 s3 -> combine (s0 :> s1 :> s2 :> s3 :> Nil)) l0 l1 l2 l3

    actual =
      withClockResetEnable @System clockGen resetGen enableGen
        $ L.filter hasTerminateFlag
        $ fmap snd
        $ WbStd.sample
          eOpts
          (driveStandard eOpts reqPairs)
          ( splitOnMask @_ @_ @1 @4
              |> repeatC (memoryWb (blockRam (C.replicate d256 0)))
              |> Circuit (\(_, ()) -> (C.repeat (), ()))
          )

  footnote $ [i| Per lane requests: #{perLaneReqs}|]
  footnote $ [i| Expected: #{expected}|]
  footnote $ [i| Actual: #{actual}|]
  assert (and $ L.zipWith3 eqWishboneS2M reqs actual expected)

--
-- stallStandard

-- | Check whether stallStandard actually stalls the outgoing bus using 'memoryWb' as subordinate.
prop_stallStandard_early :: Property
prop_stallStandard_early = property $ do
  reqs <- forAll $ genData (genWishboneTransfer @8 @4 Range.linearBounded)
  earlyBusCycle <- forAll Gen.bool
  driveStalls <- forAll $ Gen.list (Range.singleton (L.length reqs)) genSmallInt
  memStalls <- forAll $ Gen.list (Range.singleton (L.length reqs)) genSmallInt
  let
    eOpts = defExpectOptions{eoSampleMax = 10_000}
    simConfig = def{timeoutAfter = 10_000}
    reqPairs = L.zip reqs driveStalls

    (preStall, postStall) =
      withClockResetEnable @System clockGen resetGen enableGen
        $ sampleC simConfig
        $ let
            monitoredCircuit = circuit $ \wb -> do
              (wb1, preSig) <- circuitMonitor -< wb
              wb2 <- stallStandard earlyBusCycle memStalls -< wb1
              (wb3, postSig) <- circuitMonitor -< wb2
              memoryWb (blockRam (C.replicate d256 0)) -< wb3
              idC -< (preSig, postSig)
           in
            driveStandard eOpts reqPairs |> monitoredCircuit

    verifyStalls (n : stalls) trace =
      let
        active = L.dropWhile (\(pre, _) -> not (busCycle pre && strobe pre)) trace

        (stallCycles, afterStall) = L.splitAt n active

        -- busCycle asserted, strobe deasserted.
        stallOk =
          L.map
            ( \(_pre, (post, _s2m)) ->
                not (strobe post) && busCycle post == earlyBusCycle
            )
            stallCycles

        -- busCycle and strobe asserted.
        (waitCycles, termAndRest) = L.span (\(_, (_, s2m)) -> not (hasTerminateFlag s2m)) afterStall
        waitOk = L.map (\(_pre, (post, _s2m)) -> busCycle post && strobe post) waitCycles

        -- Termination cycle and remaining samples.
        (termOk, rest) = case termAndRest of
          ((_pre, (post, _s2m)) : rs) -> ([busCycle post && strobe post], rs)
          [] -> ([], [])
       in
        L.concat [stallOk, waitOk, termOk, verifyStalls stalls rest]
    verifyStalls [] _ = []

    checks = verifyStalls memStalls (L.zip (L.map fst preStall) postStall)

  footnote [i| earlyBusCycle: #{earlyBusCycle}|]
  footnote [i| memStalls: #{memStalls}|]
  assert $ and checks

tests :: TestTree
tests =
  -- TODO: Move timeout option to hedgehog for better error messages.
  -- TODO: Does not seem to work for combinatorial loops like @let x = x in x@??
  localOption (mkTimeout 60_000_000 {- 60 seconds -})
    $ localOption
      (HedgehogTestLimit (Just 1000))
      $(testGroupGenerator)
