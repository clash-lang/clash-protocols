{-# LANGUAGE RecordWildCards #-}

module Tests.Protocols.Axi4 where

-- base
import Prelude

-- clash-prelude
import Clash.Prelude qualified as C

-- extra
import Data.Proxy (Proxy (..))

-- hedgehog
import Hedgehog
import Hedgehog.Gen qualified as Gen

-- tasty
import Test.Tasty
import Test.Tasty.Hedgehog (HedgehogTestLimit (HedgehogTestLimit))
import Test.Tasty.Hedgehog.Extra (testProperty)
import Test.Tasty.TH (testGroupGenerator)

-- clash-protocols (me!)
import Protocols
import Protocols.Axi4.ReadAddress
import Protocols.Axi4.ReadData
import Protocols.Axi4.Stream
import Protocols.Axi4.WriteAddress
import Protocols.Axi4.WriteData
import Protocols.Axi4.WriteResponse
import Protocols.DfConv qualified as DfConv
import Protocols.Hedgehog

-- tests
import Tests.Protocols.Df qualified as DfTest
import Util

---------------------------------------------------------------
---------------------------- TESTS ----------------------------
---------------------------------------------------------------

type ConfAW =
  'Axi4WriteAddressConfig 'True 'True 2 2 'True 'True 'True 'True 'True 'True
type ConfW = 'Axi4WriteDataConfig 'True 2
type ConfB = 'Axi4WriteResponseConfig 'True 2
type ConfAR =
  'Axi4ReadAddressConfig 'True 'True 2 2 'True 'True 'True 'True 'True 'True
type ConfR = 'Axi4ReadDataConfig 'True 2

-- also test out the DfConv instance for Axi4Stream

prop_axi4_stream_fifo_id :: Property
prop_axi4_stream_fifo_id =
  propWithModelSingleDomain
    @C.System
    defExpectOptions
    (DfTest.genData genInfo)
    (C.exposeClockResetEnable id)
    (C.exposeClockResetEnable @C.System ckt)
    (\a b -> tally a === tally b)
 where
  ckt ::
    (C.HiddenClockResetEnable dom) =>
    Circuit
      (Axi4Stream dom ('Axi4StreamConfig 5 2 2) Int)
      (Axi4Stream dom ('Axi4StreamConfig 5 2 2) Int)
  ckt = DfConv.fifo Proxy Proxy (C.SNat @10)

  genInfo =
    Axi4StreamM2S
      <$> genVec Gen.enumBounded
      <*> genVec Gen.enumBounded
      <*> genVec Gen.enumBounded
      <*> Gen.enumBounded
      <*> Gen.enumBounded
      <*> Gen.enumBounded
      <*> DfTest.genSmallInt

tests :: TestTree
tests =
  -- TODO: Move timeout option to hedgehog for better error messages.
  -- TODO: Does not seem to work for combinatorial loops like @let x = x in x@??
  localOption (mkTimeout 12_000_000 {- 12 seconds -}) $
    localOption
      (HedgehogTestLimit (Just 1000))
      $(testGroupGenerator)

main :: IO ()
main = defaultMain tests
