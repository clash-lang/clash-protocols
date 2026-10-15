{-# LANGUAGE AllowAmbiguousTypes #-}
-- TODO: Fix warnings introduced by GHC 9.2 w.r.t. incomplete lazy pattern matches
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

{- | Ports of the tests in "Tests.Protocols.Df" and "Tests.Protocols.Df.Extra" to
the testbench-based harness in "Protocols.Experimental.Testbench". Tests have
the same names as their counterparts, so results can be compared.
-}
module Tests.Protocols.Testbench.Df where

-- base
import Data.Either (partitionEithers)
import Data.Foldable (fold)
import Data.List (mapAccumL, partition, transpose)
import Data.Maybe (catMaybes, fromMaybe)
import Prelude

-- clash-prelude
import Clash.Prelude (Vec)
import Clash.Prelude qualified as C

-- unordered-containers
import Data.HashMap.Strict qualified as HashMap

-- hedgehog
import Clash.Hedgehog.Sized.Vector (genVec)
import Hedgehog (Gen, Property, forAll, property)
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range

-- tasty
import Test.Tasty (TestTree, localOption, mkTimeout)
import Test.Tasty.Hedgehog (HedgehogTestLimit (HedgehogTestLimit))
import Test.Tasty.Hedgehog.Extra (testProperty)
import Test.Tasty.TH (testGroupGenerator)

-- text
import Data.Text qualified as Text

-- clash-protocols
import Protocols
import Protocols.Df qualified as Df
import Protocols.Experimental.Df.Extra qualified as DfExtra
import Protocols.Experimental.Testbench
import Protocols.Experimental.Testbench.Df qualified as TbDf
import Protocols.Experimental.Testbench.Scoreboard

-- tests
import Tests.Protocols.Df (
  PlusInt,
  genData,
  genMaybe,
  genSmallInt,
  genSmallPlusInt,
  genVecData,
 )
import Tests.Protocols.Testbench.Util
import Util (chunksOf, tally, tallyOn, vecFromList)

goals :: [CoverGoal]
goals = dfGoals "in" "out"

prop_id :: Property
prop_id = prop1to1 "id" goals (genData genSmallInt) id (comb idC)

prop_map :: Property
prop_map = prop1to1 "map" goals (genData genSmallInt) (map succ) (comb (Df.map succ))

prop_filter :: Property
prop_filter =
  -- Half of the values are dropped, so the output is rarely busy two cycles in a row
  prop1to1
    "filter"
    (stallGoals "in" "out")
    (genData genSmallInt)
    (filter (> 5))
    (comb (Df.filter (> 5)))

prop_catMaybes :: Property
prop_catMaybes =
  prop1to1 "catMaybes" goals (genData (genMaybe genSmallInt)) catMaybes (comb Df.catMaybes)

-- | Stares at a value for a few cycles and then forwards it
passThroughExpander ::
  forall n dom a.
  (C.KnownNat n, C.HiddenClockResetEnable dom) =>
  Circuit (Df dom a) (Df dom (Maybe a))
passThroughExpander = Df.expander (0 :: C.Index n) $ \count input ->
  let done = count == maxBound
   in ( if done then 0 else count + 1
      , if done then Just input else Nothing
      , done
      )

-- | Creates /n/ copies of a value
duplicator ::
  forall n dom a.
  (C.KnownNat n, C.HiddenClockResetEnable dom) =>
  Circuit (Df dom a) (Df dom a)
duplicator = Df.expander (0 :: C.Index n) $ \count input ->
  let done = count == maxBound
   in ( if done then 0 else count + 1
      , input
      , done
      )

-- | Sums groups of /n/ values
summer ::
  forall n dom.
  (C.KnownNat n, C.HiddenClockResetEnable dom) =>
  Circuit (Df dom Int) (Df dom Int)
summer = Df.compressor (0 :: C.Index n, 0 :: Int) $ \(count, total) input ->
  let done = count == maxBound
      total' = total + input
   in ( if done then (0, 0) else (count + 1, total')
      , if done then Just total' else Nothing
      )

testExpanderPassThrough :: forall n. (C.KnownNat n) => String -> C.SNat n -> Property
testExpanderPassThrough name C.SNat =
  prop1to1
    name
    -- Values are only forwarded every /n/ cycles
    (if C.natToNum @n == (1 :: Int) then goals else stallGoals "in" "out")
    (genData genSmallInt)
    id
    (hidden (passThroughExpander @n |> Df.catMaybes))

prop_expander_passthrough_linerate :: Property
prop_expander_passthrough_linerate = testExpanderPassThrough "expander_passthrough_linerate" C.d1

prop_expander_passthrough_slow :: Property
prop_expander_passthrough_slow = testExpanderPassThrough "expander_passthrough_slow" C.d4

testExpanderDuplicate :: forall n. (C.KnownNat n) => String -> C.SNat n -> Property
testExpanderDuplicate name duplication =
  prop1to1
    name
    goals
    (genData genSmallInt)
    (concatMap (replicate (C.snatToNum duplication)))
    (hidden (duplicator @n))

prop_expander_duplicate_linerate :: Property
prop_expander_duplicate_linerate = testExpanderDuplicate "expander_duplicate_linerate" C.d1

prop_expander_duplicate_slow :: Property
prop_expander_duplicate_slow = testExpanderDuplicate "expander_duplicate_slow" C.d4

testCompressorSum :: forall n. (C.KnownNat n) => String -> C.SNat n -> Property
testCompressorSum name batchSize =
  prop1to1 name goals' (genData genSmallInt) referenceImpl (hidden (summer @n))
 where
  chunk = C.snatToNum batchSize
  -- Values are only produced every /n/ inputs
  goals' = if chunk == (1 :: Int) then goals else stallGoals "in" "out"
  -- Given [a,b,c,d,e] and chunk = 2, yield [a+b,c+d]
  referenceImpl =
    map sum . takeWhile ((== chunk) . length) . map (take chunk) . iterate (drop chunk)

prop_compressor_sum_linerate :: Property
prop_compressor_sum_linerate = testCompressorSum "compressor_sum_linerate" C.d1

prop_compressor_sum_slow :: Property
prop_compressor_sum_slow = testCompressorSum "compressor_sum_slow" C.d4

prop_registerFwd :: Property
prop_registerFwd =
  prop1to1 "registerFwd" goals (genData genSmallInt) id (hidden Df.registerFwd)

prop_registerBwd :: Property
prop_registerBwd =
  prop1to1 "registerBwd" goals (genData genSmallInt) id (hidden Df.registerBwd)

prop_fanout1 :: Property
prop_fanout1 =
  prop1toN
    "fanout1"
    (dfGoals "in" "out0")
    (genData genSmallInt)
    C.repeat
    (hidden (Df.fanout @1))

prop_fanout2 :: Property
prop_fanout2 =
  prop1toN
    "fanout2"
    (dfGoals "in" "out1")
    (genData genSmallInt)
    C.repeat
    (hidden (Df.fanout @2))

prop_fanout7 :: Property
prop_fanout7 =
  prop1toN
    "fanout7"
    (dfGoals "in" "out6")
    (genData genSmallInt)
    C.repeat
    (hidden (Df.fanout @7))

prop_roundrobin :: Property
prop_roundrobin =
  prop1toN
    "roundrobin"
    -- Outputs take turns, so a single output never transfers back to back
    (stallGoals "in" "out2")
    (genData genSmallInt)
    chunksOf
    (hidden (Df.roundrobin @3))

prop_roundrobinCollectNoSkip :: Property
prop_roundrobinCollectNoSkip =
  propNto1
    "roundrobinCollectNoSkip"
    (dfGoals "in0" "out")
    (genVecData genSmallInt)
    (concat . transpose . C.toList)
    (hidden (Df.roundrobinCollect @3 Df.NoSkip))

{- | For collect modes in which the order of outputs depends on timing, the
output is checked with a scoreboard. Monitors on the inputs of the design under
test feed it the values that were actually accepted, and the scoreboard checks
that every one of them shows up exactly once at the output.
-}
tbCollectUnordered ::
  Df.CollectMode ->
  Vec 3 TbDf.DriverOptions ->
  TbDf.SinkOptions ->
  Vec 3 [Int] ->
  Circuit () TraceTree
tbCollectUnordered mode dOpts sOpts xss =
  testbench (C.toList drvTs <> C.toList monTs <> [outMonT, sinkT, sbT])
 where
  (drvTs, as) = drivers "in" dOpts xss monAcks
  monitors = monitor1 <$> C.indicesI <*> as <*> C.lazyV dutAcks
  monitor1 i a ack =
    runC (TbDf.monitor ("dut_in" <> show i) tbClock tbReset) a ((), (), ack)
  monAcks = fst <$> monitors
  (monTs, inTxs, as') = C.unzip3 (snd <$> monitors)
  (dutAcks, b) = runC (instantiate (hidden (Df.roundrobinCollect @3 mode))) as' bAck
  (bAck, (outMonT, outTxs, b')) = runC (TbDf.monitor "dut_out" tbClock tbReset) b ((), (), sinkAck)
  (sinkAck, sinkT) = runC (TbDf.sink "sink" tbClock tbReset sOpts) b' ()
  (_, inTxList) = runC transactionsVec inTxs ()
  (_, outTxList) = runC transactions outTxs ()
  (_, sbT) =
    runC
      ( scoreboard
          "scoreboard"
          tbClock
          defScoreboardOptions{order = OutOfOrder}
          []
          ()
          (\s (_, x) -> (s, [x]))
      )
      (inTxList, outTxList)
      ()

propCollectUnordered :: String -> Df.CollectMode -> Property
propCollectUnordered name mode = property $ do
  xss <- forAll (genVecData genSmallInt)
  dOpts <- forAll (genVec TbDf.genDriverOptions)
  sOpts <- forAll TbDf.genSinkOptions
  assertTb name (dfGoals "dut_in0" "dut_out") (tbCollectUnordered mode dOpts sOpts xss)

prop_roundrobinCollectSkip :: Property
prop_roundrobinCollectSkip = propCollectUnordered "roundrobinCollectSkip" Df.Skip

prop_roundrobinCollectParallel :: Property
prop_roundrobinCollectParallel = propCollectUnordered "roundrobinCollectParallel" Df.Parallel

prop_unbundleVec :: Property
prop_unbundleVec =
  prop1toN
    "unbundleVec"
    (dfGoals "in" "out0")
    (fmap C.repeat <$> genData genSmallInt)
    (vecFromList . transpose . map C.toList)
    (hidden (Df.unbundleVec @3 @_ @Int))

prop_bundleVec :: Property
prop_bundleVec =
  propNto1
    "bundleVec"
    (dfGoals "in0" "out")
    (C.repeat <$> genData genSmallPlusInt)
    (map vecFromList . transpose . C.toList)
    (comb (Df.bundleVec @3 @_ @PlusInt))

prop_fanin :: Property
prop_fanin =
  propNto1
    "fanin"
    (dfGoals "in0" "out")
    (genVecData genSmallInt)
    (map sum . transpose . C.toList)
    (comb (Df.fanin @3 @_ @Int (+)))

prop_mfanin :: Property
prop_mfanin =
  propNto1
    "mfanin"
    (dfGoals "in0" "out")
    (genVecData genSmallPlusInt)
    (map fold . transpose . C.toList)
    (comb (Df.mfanin @3 @_ @PlusInt))

genEqualLength :: Gen ([Int], [Int])
genEqualLength = do
  as <- genData genSmallInt
  bs <- genData genSmallInt
  let n = min (length as) (length bs)
  pure (take n as, take n bs)

prop_zipWith :: Property
prop_zipWith =
  prop2to1
    "zipWith"
    (dfGoals "in0" "out")
    genEqualLength
    (uncurry (zipWith (+)))
    (comb (Df.zipWith @_ @Int @Int (+)))

prop_zip :: Property
prop_zip =
  prop2to1
    "zip"
    (dfGoals "in0" "out")
    genEqualLength
    (uncurry zip)
    (comb (Df.zip @Int @Int))

prop_partition :: Property
prop_partition =
  prop1to2
    "partition"
    (stallGoals "in" "out0")
    (genData genSmallInt)
    (partition (> 5))
    (comb (Df.partition @_ @Int (> 5)))

prop_partitionEithers :: Property
prop_partitionEithers =
  prop1to2
    "partitionEithers"
    -- 'Gen.either' mostly generates 'Right's
    (dfGoals "in" "out1")
    (genData (Gen.either genSmallInt Gen.alphaNum))
    partitionEithers
    (comb (Df.partitionEithers @_ @Int @Char))

prop_route :: Property
prop_route =
  prop1toN
    "route"
    -- Every destination should be used regularly
    ( stallGoals "in" "out0"
        <> [goal ("out" <> show i <> "/transfer") 15 | i <- [0 .. 2 :: Int]]
    )
    (zip <$> genData Gen.enumBounded <*> genData genSmallInt)
    (\inp -> C.map (\i -> map snd (filter ((== i) . fst) inp)) C.indicesI)
    (comb (Df.route @3 @_ @Int))

genSelect :: Gen (Vec 3 [Int], [C.Index 3])
genSelect = do
  n <- genSmallInt
  ixs <- Gen.list (Range.singleton n) Gen.enumBounded
  let tall i = fromMaybe 0 (HashMap.lookup i (tally ixs))
  dats <- mapM (\i -> Gen.list (Range.singleton (tall i)) genSmallInt) C.indicesI
  pure (dats, ixs)

prop_select :: Property
prop_select =
  propSelect
    "select"
    (dfGoals "in0" "out")
    genSelect
    (snd . uncurry (mapAccumL goModel))
    (comb (Df.select @3 @_ @Int))
 where
  goModel :: Vec 3 [Int] -> C.Index 3 -> (Vec 3 [Int], Int)
  goModel vec ix = let (i : is) = vec C.!! ix in (C.replace ix is vec, i)

genSelectN :: Gen (Vec 3 [Int], [(C.Index 3, C.Index 10)])
genSelectN = do
  n <- genSmallInt
  ixs <- Gen.list (Range.singleton n) Gen.enumBounded
  lengths <- Gen.list (Range.singleton n) Gen.enumBounded
  let tallied = tallyOn fst (fromIntegral . snd) (zip ixs lengths)
      tall i = fromMaybe 0 (HashMap.lookup i tallied)
  dats <- mapM (\i -> Gen.list (Range.singleton (tall i)) genSmallInt) C.indicesI
  pure (dats, zip ixs lengths)

{- | 'Df.selectN' acknowledges selections of zero values during reset, which violates
the 'Df' invariants. Its selections are therefore never driven during reset.
-}
prop_selectN :: Property
prop_selectN = property $ do
  (xss, ss) <- forAll genSelectN
  dOpts <- forAll (genVec TbDf.genDriverOptions)
  sOpts <- forAll TbDf.genDriverOptions
  eOpts <- forAll TbDf.genExpectOptions
  assertTb
    "selectN"
    (dfGoals "in0" "out")
    ( tbSelect
        (hidden (Df.selectN @3 @10 @_ @Int))
        dOpts
        sOpts{TbDf.driveDuringReset = False}
        eOpts
        xss
        ss
        (concat (snd (mapAccumL goModel xss ss)))
    )
 where
  goModel :: Vec 3 [Int] -> (C.Index 3, C.Index 10) -> (Vec 3 [Int], [Int])
  goModel vec (ix, len) =
    let (as, bs) = splitAt (fromIntegral len) (vec C.!! ix)
     in (C.replace ix bs vec, as)

genSelectUntil :: Gen (Vec 3 [(Int, Bool)], [C.Index 3])
genSelectUntil = do
  n <- genSmallInt
  ixs <- Gen.list (Range.singleton n) Gen.enumBounded
  dats <- mapM (\i -> goChannelInput (HashMap.lookup i (tally ixs))) C.indicesI
  pure (dats, ixs)
 where
  goChannelInput :: Maybe Int -> Gen [(Int, Bool)]
  goChannelInput Nothing = pure []
  goChannelInput (Just n) = do
    inputs0 <- Gen.list (Range.singleton n) (Gen.list (Range.linear 1 10) genSmallInt)
    let tagEnd xs = zip (init xs) (repeat False) <> [(last xs, True)]
    pure (concatMap tagEnd inputs0)

prop_selectUntil :: Property
prop_selectUntil =
  propSelect
    "selectUntil"
    (dfGoals "in0" "out")
    genSelectUntil
    (concat . snd . uncurry (mapAccumL goModel))
    (comb (Df.selectUntil @3 @_ @(Int, Bool) snd))
 where
  goModel :: Vec 3 [(Int, Bool)] -> C.Index 3 -> (Vec 3 [(Int, Bool)], [(Int, Bool)])
  goModel vec ix =
    let (as, b : bs) = break snd (vec C.!! ix)
     in (C.replace ix bs vec, as <> [b])

prop_fifo :: Property
prop_fifo =
  prop1to1
    "fifo"
    -- At most ten values are sent, so the FIFO never fills up and never stalls its input
    (dfGoals "" "out" <> [goal "out/long_stall" 10])
    (genData genSmallInt)
    id
    (hidden (Df.fifo (C.SNat @10)))

{- | 'Df.unsafeFromMaybe' drops values when its output is not ready, and counts how
many it dropped. Values dropped while a value waits to be acknowledged come
after it, so every received value must be the next value sent after skipping
the values dropped while the previous value waited. A monitor extracts the
transferred values, which are checked together with the number of dropped values
at the moment of transfer.
-}
prop_toMaybeFromMaybe :: Property
prop_toMaybeFromMaybe = property $ do
  xs <- forAll (genData genSmallInt)
  dOpts <- forAll TbDf.genDriverOptions
  sOpts <- forAll TbDf.genSinkOptions
  -- The design acknowledges every value, and drops them if it cannot pass them on
  assertTb "toMaybeFromMaybe" (stallGoals "" "out") (tb xs dOpts sOpts)
 where
  tb xs dOpts sOpts = testbench [drvT, monT, sinkT, checkT]
   where
    (_, (drvT, a)) = runC (TbDf.driver "in" tbClock tbReset (duringReset dut dOpts) xs) () ((), aAck)
    (aAck, (b, nDropped)) = runC (instantiate dut) a (bAck, ())
    (bAck, (monT, txs, b')) = runC (TbDf.monitor "out" tbClock tbReset) b ((), (), sinkAck)
    (sinkAck, sinkT) = runC (TbDf.sink "sink" tbClock tbReset sOpts) b' ()
    txsWithDropped = (\tx n -> [(x, n) | Just x <- [tx]]) <$> txs <*> nDropped
    (_, checkT) = runC (checker "check" tbClock 16 (xs, 0, 0) check (const True)) txsWithDropped ()

  dut :: Dut (Df C.System Int) (Df C.System Int, CSignal C.System (C.Unsigned 64))
  dut = hidden (Df.forceResetSanity |> Df.toMaybe |> Df.unsafeFromMaybe)

  -- State: values not received yet, number of dropped values at the previous
  -- transfer, and number of values dropped while the previous value waited.
  check ::
    ([Int], C.Unsigned 64, C.Unsigned 64) ->
    (Int, C.Unsigned 64) ->
    Either Text.Text ([Int], C.Unsigned 64, C.Unsigned 64)
  check (sent, nDroppedPrev, skip) (received, nDroppedNow) =
    case drop (fromIntegral skip) sent of
      s : ss
        | s == received -> Right (ss, nDroppedNow, nDroppedNow - nDroppedPrev)
        | otherwise -> Left (Text.pack ("expected " <> show s <> ", got " <> show received))
      [] -> Left (Text.pack ("received " <> show received <> ", but nothing more was sent"))

-- | 'DfExtra.singleEntryBuffer', ignoring its ready signal
singleEntryBufferDropReady ::
  (C.HiddenClockResetEnable dom) =>
  Circuit (Df dom Int) (Df dom Int)
singleEntryBufferDropReady =
  DfExtra.singleEntryBuffer |> Circuit (\(~(dat, _ready), ack) -> ((ack, ()), dat))

prop_singleEntryBuffer :: Property
prop_singleEntryBuffer =
  -- Inserts a bubble after every transfer
  prop1to1
    "singleEntryBuffer"
    (stallGoals "in" "out")
    (genData genSmallInt)
    id
    (hidden singleEntryBufferDropReady)

prop_bypassFifo :: Property
prop_bypassFifo =
  prop1to1
    "bypassFifo"
    goals
    (genData genSmallInt)
    id
    (hidden (DfExtra.bypassFifo C.d1 (Df.fifo C.d8)))

prop_stallNext :: Property
prop_stallNext = property $ do
  stalls <- forAll (Gen.list (Range.linear 0 100) Gen.bool)
  xs <- forAll (genData genSmallInt)
  dOpts <- forAll TbDf.genDriverOptions
  eOpts <- forAll TbDf.genExpectOptions
  let dut = hidden (DfExtra.stallNext (C.fromList (stalls <> repeat True)))
  assertTb "stallNext" goals (tb1to1 dut dOpts eOpts xs xs)

tests :: TestTree
tests =
  -- TODO: Move timeout option to CI only
  localOption (mkTimeout 120_000_000) $
    localOption
      (HedgehogTestLimit (Just 1000))
      $(testGroupGenerator)
