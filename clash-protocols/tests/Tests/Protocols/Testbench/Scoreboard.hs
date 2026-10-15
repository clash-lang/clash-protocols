{-# LANGUAGE OverloadedStrings #-}

-- | Tests of "Protocols.Experimental.Testbench.Scoreboard"
module Tests.Protocols.Testbench.Scoreboard where

import Prelude

import Clash.Prelude (Signal, System, Vec (Nil, (:>)))
import Clash.Prelude qualified as C
import Data.Text (Text)
import Protocols (Circuit (..))
import Protocols.Experimental.Testbench
import Protocols.Experimental.Testbench.Scoreboard
import Test.Tasty (TestTree)
import Test.Tasty.HUnit (Assertion, testCase, (@?=))
import Test.Tasty.TH (testGroupGenerator)

-- | Transactions per cycle, none after the given list
txs :: [[a]] -> Signal System [a]
txs xs = C.fromList_lazy (xs <> repeat [])

-- | A scoreboard expecting every input to show up at the output
identity :: ScoreboardOptions -> [Int] -> [[Int]] -> [[Int]] -> IO TestResult
identity opts expected0 ins outs =
  runTraceTree defTestOptions{timeout = Just (cycles @System 1000)} (traceGroup "tb" [tree])
 where
  Circuit sb = scoreboard "sb" C.clockGen opts expected0 () (\s i -> (s, [i]))
  (_, tree) = sb ((txs ins, txs outs), ())

case_inOrder :: Assertion
case_inOrder = do
  r <- identity defScoreboardOptions [] [[1], [2], [3]] [[], [1], [2], [3]]
  r.outcome @?= Passed
  -- The last output arrives in cycle 3, after which the scoreboard settles
  r.endTime @?= cycles @System (3 + 16)

case_sameCycle :: Assertion
case_sameCycle = do
  r <- identity defScoreboardOptions [] [[1, 2], [3]] [[1], [2, 3]]
  r.outcome @?= Passed

case_outOfOrder :: Assertion
case_outOfOrder = do
  r <-
    identity
      defScoreboardOptions{order = OutOfOrder}
      []
      [[1], [2], [3]]
      [[], [], [3], [1], [2]]
  r.outcome @?= Passed

case_inOrderMismatch :: Assertion
case_inOrderMismatch = do
  r <- identity defScoreboardOptions [] [[1], [2], [3]] [[], [1], [3], [2]]
  r.outcome
    @?= Failed{source = "tb/sb/status", message = "output #1: expected 2, got 3; pending: [2, 3]"}
  r.endTime @?= cycles @System 2

case_outOfOrderMismatch :: Assertion
case_outOfOrderMismatch = do
  r <- identity defScoreboardOptions{order = OutOfOrder} [] [[1], [2]] [[], [2], [2]]
  r.outcome
    @?= Failed{source = "tb/sb/status", message = "output #1: unexpected output 2; pending: [1]"}

case_unexpectedOutput :: Assertion
case_unexpectedOutput = do
  r <- identity defScoreboardOptions [] [[1]] [[], [1], [], [], [7]]
  r.outcome
    @?= Failed
      { source = "tb/sb/status"
      , message = "output #1: unexpected output 7: no outputs pending"
      }
  r.endTime @?= cycles @System 4

-- | Outputs arriving after the scoreboard settled are not noticed
case_afterSettling :: Assertion
case_afterSettling = do
  r <-
    identity
      defScoreboardOptions{settleCycles = 2}
      []
      [[1]]
      ([[1]] <> replicate 5 [] <> [[7]])
  r.outcome @?= Passed
  r.endTime @?= cycles @System 2

case_expectedUpFront :: Assertion
case_expectedUpFront = do
  r <- identity defScoreboardOptions [5, 6] [] [[], [5], [], [6]]
  r.outcome @?= Passed

case_pendingAtTimeout :: Assertion
case_pendingAtTimeout = do
  r <- identity defScoreboardOptions [] [[1], [2]] [[], [1]]
  r.outcome @?= TimedOut{blockedBy = ["tb/sb/status"]}

-- | A checker that expects increasing values, and is done after seeing a 3
increasing :: [[Int]] -> IO TestResult
increasing outs =
  runTraceTree defTestOptions{timeout = Just (cycles @System 1000)} (traceGroup "tb" [tree])
 where
  Circuit chk = checker "check" C.clockGen 0 0 step (>= 3)
  (_, tree) = chk (txs outs, ())
  step :: Int -> Int -> Either Text Int
  step prev new
    | new > prev = Right new
    | otherwise = Left "not increasing"

case_checker :: Assertion
case_checker = do
  r0 <- increasing [[1], [], [2, 3]]
  r0.outcome @?= Passed
  r0.endTime @?= cycles @System 2
  r1 <- increasing [[1], [1]]
  r1.outcome @?= Failed{source = "tb/check/status", message = "output #1: not increasing"}
  r2 <- increasing [[1]]
  r2.outcome @?= TimedOut{blockedBy = ["tb/check/status"]}

case_transactionsVec :: Assertion
case_transactionsVec = do
  let
    Circuit f = transactionsVec @3 @System @Int
    inputs =
      C.fromList_lazy [Just 1, Nothing, Just 3]
        :> C.fromList_lazy [Nothing, Nothing, Just 4]
        :> C.fromList_lazy [Just 2, Nothing, Nothing]
        :> Nil
    (_, out) = f (inputs, ())
  C.sampleN 3 out @?= [[(0, 1), (2, 2)], [], [(0, 3), (1, 4)]]

tests :: TestTree
tests = $(testGroupGenerator)
