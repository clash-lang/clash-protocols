module Tests.Protocols.Driver where

import Clash.Explicit.Prelude (System, clockGen, enableGen, resetGen)

import Control.Monad.Trans.Class (lift)

import Clash.Prelude (withClockResetEnable)
import Hedgehog
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range
import Protocols.Df (Df, consume)
import Protocols.Experimental.Hedgehog.Driver (CircuitDriverT, runCircuitDriverT, step)
import Protocols.Internal (Ack (..))
import Test.Tasty (TestTree, localOption, mkTimeout)
import Test.Tasty.HUnit (Assertion, testCase, (@?=))
import Test.Tasty.Hedgehog (HedgehogTestLimit (HedgehogTestLimit), testProperty)
import Test.Tasty.TH (testGroupGenerator)

case_df_circ :: Assertion
case_df_circ = do
  res <- runCircuitDriverT circ driver
  res @?= "done"
 where
  withCRE = withClockResetEnable @System clockGen resetGen enableGen
  circ = withCRE consume
  driver :: CircuitDriverT System (Df System Bool) IO String
  driver = do
    res0 <- step Nothing
    lift $ res0 @?= Ack True
    res1 <- step $ Just False
    lift $ res1 @?= Ack True
    res2 <- step $ Just True
    lift $ res2 @?= Ack True
    pure "done"

prop_df_prop_circ :: Property
prop_df_prop_circ = property $ do
  res <- runCircuitDriverT circ driver
  res === "done"
 where
  withCRE = withClockResetEnable @System clockGen resetGen enableGen
  circ = withCRE consume

  genMaybe genA = Gen.choice [Gen.constant Nothing, Just <$> genA]
  genInt a b = Gen.integral (Range.linear a b)

  driver :: CircuitDriverT System (Df System Int) (PropertyT IO) String
  driver = do
    req <- lift $ forAll $ genMaybe (genInt 0 100)
    res0 <- step req
    lift $ res0 === Ack True
    pure "done"

tests :: TestTree
tests =
  -- TODO: Move timeout option to hedgehog for better error messages.
  -- TODO: Does not seem to work for combinatorial loops like @let x = x in x@??
  localOption (mkTimeout 20_000_000 {- 20 seconds -}) $
    localOption
      (HedgehogTestLimit (Just 1000))
      $(testGroupGenerator)
