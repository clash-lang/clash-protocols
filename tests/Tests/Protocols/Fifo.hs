{-# LANGUAGE NumericUnderscores #-}
{-# OPTIONS_GHC -fno-warn-orphans #-} -- Hashable (Index n)

module Tests.Protocols.Fifo where

-- base
import Prelude

-- clash-prelude
import qualified Clash.Prelude as C

-- extra
import Data.Proxy (Proxy(..))

-- hedgehog
import Hedgehog

-- tasty
import Test.Tasty
import Test.Tasty.Hedgehog (HedgehogTestLimit(HedgehogTestLimit))
import Test.Tasty.Hedgehog.Extra (testProperty)
import Test.Tasty.TH (testGroupGenerator)

-- clash-protocols (me!)
import Protocols
import Protocols.Hedgehog
import Protocols.Fifo (fifo)

-- tests
import Util
import qualified Tests.Protocols.Df as DfTest

---------------------------------------------------------------
---------------------------- TESTS ----------------------------
---------------------------------------------------------------
prop_df_fifo_id :: Property
prop_df_fifo_id = propWithModelSingleDomain
               @C.System
               defExpectOptions
               (DfTest.genData DfTest.genSmallInt)
               (C.exposeClockResetEnable id)
               (C.exposeClockResetEnable @C.System ckt)
               (\a b -> tally a === tally b)
  where
  ckt :: (C.HiddenClockResetEnable dom) => Circuit (Df dom Int) (Df dom Int)
  ckt = Circuit (fifo (Proxy @(_, _, Int)) Proxy (C.SNat @10) () ())


tests :: TestTree
tests =
    -- TODO: Move timeout option to hedgehog for better error messages.
    -- TODO: Does not seem to work for combinatorial loops like @let x = x in x@??
    localOption (mkTimeout 12_000_000 {- 12 seconds -})
  $ localOption (HedgehogTestLimit (Just 1000))
  $(testGroupGenerator)

main :: IO ()
main = defaultMain tests
