module Tests.Protocols.Driver where

import Clash.Explicit.Prelude (SNat (SNat), System, clockGen, enableGen, resetGen)

import Control.Monad.Trans.Class (lift)

import Clash.Prelude (withClockResetEnable)
import Hedgehog
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range
import Protocols.Df (Df, consume, fifo)
import Protocols.Experimental.Hedgehog.Driver (CircuitDriverT, runCircuitDriverT, step)
import Protocols.Internal (Ack (..))
import Test.Tasty (TestTree, localOption, mkTimeout)
import Test.Tasty.HUnit (Assertion, testCase, (@?=))
import Test.Tasty.Hedgehog (HedgehogTestLimit (HedgehogTestLimit), testProperty)
import Test.Tasty.TH (testGroupGenerator)

--
-- Test circuit we'll be using here
--

case_df_fifo_state :: Assertion
case_df_fifo_state = do
  res <- runCircuitDriverT circ () () () driver
  res @?= "done"
 where
  withCRE = withClockResetEnable @System clockGen resetGen enableGen
  circ = withCRE fifo (SNat @10)
  driver :: CircuitDriverT System (Df System Int) (Df System Int) IO String
  driver = do
    -- test beginning
    (a0, b0) <- step Nothing (Ack False)
    lift $ a0 @?= Ack False
    lift $ b0 @?= Nothing
    -- queue two numbers
    (a1, req1) <- step (Just 2) (Ack False)
    lift $ a1 @?= Ack True
    lift $ req1 @?= Just 2
    (a2, req2) <- step (Just 3) (Ack False)
    lift $ a2 @?= Ack True
    lift $ req2 @?= Just 2

    -- acknowledge two numbers, make sure they are in the right order

    -- first look at the output
    (a3, req3) <- step Nothing (Ack False)
    lift $ a3 @?= Ack False
    lift $ req3 @?= Just 2

    -- got the first number
    -- acknowledge that

    (a4, req4) <- step Nothing (Ack True)
    lift $ a4 @?= Ack False
    lift $ req4 @?= Just 2

    -- get the second number
    (a5, req5) <- step Nothing (Ack False)
    lift $ a5 @?= Ack False
    lift $ req5 @?= Just 3

    -- ack
    (a6, req6) <- step Nothing (Ack True)
    lift $ a6 @?= Ack False
    lift $ req6 @?= Just 3

    -- no more data
    (a7, req7) <- step Nothing (Ack False)
    lift $ a7 @?= Ack False
    lift $ req7 @?= Nothing

    pure "done"

case_df_circ :: Assertion
case_df_circ = do
  res <- runCircuitDriverT circ () () () driver
  res @?= "done"
 where
  withCRE = withClockResetEnable @System clockGen resetGen enableGen
  circ = withCRE consume
  driver :: CircuitDriverT System (Df System Bool) () IO String
  driver = do
    (res0, ()) <- step Nothing ()
    lift $ res0 @?= Ack True
    (res1, ()) <- step (Just False) ()
    lift $ res1 @?= Ack True
    (res2, ()) <- step (Just True) ()
    lift $ res2 @?= Ack True
    pure "done"

prop_df_prop_circ :: Property
prop_df_prop_circ = property $ do
  res <- runCircuitDriverT circ () () () driver
  res === "done"
 where
  withCRE = withClockResetEnable @System clockGen resetGen enableGen
  circ = withCRE consume

  driver :: CircuitDriverT System (Df System Int) () (PropertyT IO) String
  driver = do
    req <- lift $ forAll $ genMaybe (genInt 0 100)
    (res0, ()) <- step req ()
    lift $ res0 === Ack True
    pure "done"

genMaybe :: (MonadGen m) => m a -> m (Maybe a)
genMaybe genA = Gen.choice [Gen.constant Nothing, Just <$> genA]

genInt :: (MonadGen m, Integral a) => a -> a -> m a
genInt a b = Gen.integral (Range.linear a b)

genList :: (MonadGen m) => Int -> m a -> m [a]
genList maxLen genA = Gen.list (Range.linear 0 maxLen) genA

tests :: TestTree
tests =
  -- TODO: Move timeout option to hedgehog for better error messages.
  -- TODO: Does not seem to work for combinatorial loops like @let x = x in x@??
  localOption (mkTimeout 20_000_000 {- 20 seconds -}) $
    localOption
      (HedgehogTestLimit (Just 1000))
      $(testGroupGenerator)
