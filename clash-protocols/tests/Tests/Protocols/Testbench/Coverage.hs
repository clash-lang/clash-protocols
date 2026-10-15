{-# LANGUAGE OverloadedStrings #-}

-- | Tests of coverage in "Protocols.Experimental.Testbench"
module Tests.Protocols.Testbench.Coverage where

import Prelude

import Clash.Prelude (Signal, System)
import Clash.Prelude qualified as C
import Data.Map qualified as Map
import Hedgehog qualified as H
import Hedgehog.Gen qualified as Gen
import Hedgehog.Internal.Property (Property (..))
import Hedgehog.Internal.Report (Report (..))
import Hedgehog.Internal.Report qualified as Report
import Hedgehog.Internal.Runner (checkReport)
import Hedgehog.Internal.Seed qualified as Seed
import Protocols (Circuit (..))
import Protocols.Experimental.Testbench
import Protocols.Experimental.Testbench.Hedgehog (assertTestbench)
import Test.Tasty (TestTree)
import Test.Tasty.HUnit (Assertion, assertBool, testCase, (@?=))
import Test.Tasty.TH (testGroupGenerator)

-- | Run a property without printing anything, and return whether it passed
quietCheck :: H.Property -> IO Bool
quietCheck prop = do
  seed <- Seed.random
  report <- checkReport (propertyConfig prop) 0 seed (propertyTest prop) (const (pure ()))
  pure $ case reportStatus report of
    Report.OK -> True
    _ -> False

-- | 'PreventStop' for the given number of cycles, 'StopOk' afterwards
stopAfter :: Int -> Signal System Status
stopAfter n = C.fromList_lazy (replicate n PreventStop <> repeat StopOk)

-- | A testbench running for the given number of cycles, with some cover points
testbench :: Int -> [TraceTree] -> Circuit () TraceTree
testbench n trees = Circuit $ \_ -> ((), traceGroup "tb" (traceStatus "status" (stopAfter n) : trees))

case_counts :: Assertion
case_counts = do
  let hits = C.fromList_lazy ([False, True, True, False, True] <> repeat False)
  r <- runTestbench defTestOptions (testbench 6 [cover @System "hit" hits])
  r.coverage @?= Map.fromList [("tb/hit", 3)]

data Parity = Even | Odd
  deriving (Show, Enum, Bounded)

case_coverEnum :: Assertion
case_coverEnum = do
  let ords = C.fromList_lazy ([Nothing, Just LT, Just GT, Just LT] <> repeat Nothing)
  r <- runTestbench defTestOptions (testbench 5 [coverEnum @System "ord" ords])
  r.coverage @?= Map.fromList [("tb/ord/LT", 2), ("tb/ord/EQ", 0), ("tb/ord/GT", 1)]

case_coverTransactions :: Assertion
case_coverTransactions = do
  let
    values = C.fromList_lazy ([Just 1, Nothing, Just 3, Just (5 :: Int)] <> repeat Nothing)
    Circuit f = coverTransactions @System "parity" (\x -> if even x then Even else Odd)
    (_, tree) = f (values, ())
  r <- runTestbench defTestOptions (testbench 5 [tree])
  r.coverage @?= Map.fromList [("tb/parity/Even", 0), ("tb/parity/Odd", 3)]

-- | Undefined cover points do not count as hit
case_undefinedCover :: Assertion
case_undefinedCover = do
  let hits = C.fromList_lazy ([True, C.errorX "undefined", True] <> repeat False)
  r <- runTestbench defTestOptions (testbench 3 [cover @System "hit" hits])
  r.coverage @?= Map.fromList [("tb/hit", 2)]

-- | A testbench hitting a cover point if the generated 'Bool' is 'True'
propCover :: [CoverGoal] -> H.Property
propCover goals = H.withTests 100 $ H.property $ do
  hit <- H.forAll Gen.bool
  assertTestbench defTestOptions goals (testbench 2 [cover @System "hit" (pure hit)])

case_goals :: Assertion
case_goals = do
  quietCheck (propCover []) >>= assertBool "no goals"
  quietCheck (propCover [CoverGoal "tb/hit" 10]) >>= assertBool "reachable goal"
  quietCheck (propCover [CoverGoal "tb/hit" 100]) >>= assertBool "unreachable goal" . not
  quietCheck (propCover [CoverGoal "tb/typo" 0]) >>= assertBool "unknown cover point" . not

tests :: TestTree
tests = $(testGroupGenerator)
