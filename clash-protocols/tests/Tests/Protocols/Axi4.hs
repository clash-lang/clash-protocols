{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE RecordWildCards #-}

module Tests.Protocols.Axi4 where

-- base
import Prelude

-- clash-prelude
import qualified Clash.Prelude as C

-- extra
import Data.Proxy (Proxy (..))

-- hedgehog
import Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

-- strict-tuple
import Data.Tuple.Strict (T3 (..), T4 (..))

-- tasty
import Test.Tasty
import Test.Tasty.Hedgehog (HedgehogTestLimit (HedgehogTestLimit))
import Test.Tasty.Hedgehog.Extra (testProperty)
import Test.Tasty.TH (testGroupGenerator)

-- clash-protocols (me!)
import Protocols
import Protocols.Axi4.Common
import Protocols.Axi4.ReadAddress
import Protocols.Axi4.ReadData
import Protocols.Axi4.Stream
import Protocols.Axi4.WriteAddress
import Protocols.Axi4.WriteData
import Protocols.Axi4.WriteResponse
import qualified Protocols.Df as Df
import qualified Protocols.DfConv as DfConv
import Protocols.Hedgehog
import Protocols.Internal

-- tests

import qualified Tests.Protocols.Df as DfTest
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

prop_axi4_convert_write_id :: Property
prop_axi4_convert_write_id =
  DfTest.idWithModelDf
    defExpectOptions
    (DfTest.genData genInfo)
    id
    ( C.withClockResetEnable @C.System C.clockGen C.resetGen C.enableGen $
        DfConv.dfConvTestBench
          Proxy
          Proxy
          (repeat True)
          (repeat $ Df.Data (toKeepType ROkay, 0))
          ckt
    )
 where
  ckt ::
    (C.HiddenClockResetEnable dom) =>
    Circuit
      ( Axi4WriteAddress dom ConfAW Int
      , Axi4WriteData dom ConfW Int
      , Reverse (Axi4WriteResponse dom ConfB Int)
      )
      ( Axi4WriteAddress dom ConfAW Int
      , Axi4WriteData dom ConfW Int
      , Reverse (Axi4WriteResponse dom ConfB Int)
      )
  ckt = DfConv.convert Proxy Proxy

  genInfo =
    (,,,,)
      <$> genWriteAddrInfo
      <*> genBurstLen
      <*> genBurst
      <*> genStrobe
      <*> DfTest.genSmallInt
  genWriteAddrInfo =
    Axi4WriteAddressInfo
      <$> Gen.enumBounded
      <*> Gen.enumBounded
      <*> (toKeepType <$> Gen.enumBounded)
      <*> ( toKeepType
              <$> ( pure Bs1
                      C.<|> pure Bs2
                      C.<|> pure Bs4
                      C.<|> pure Bs8
                      C.<|> pure Bs16
                      C.<|> pure Bs32
                      C.<|> pure Bs64
                      C.<|> pure Bs128
                  )
          )
      <*> (toKeepType <$> (pure NonExclusiveAccess C.<|> pure ExclusiveAccess))
      <*> ( toKeepType
              <$> ( T4
                      <$> (pure NonBufferable C.<|> pure Bufferable)
                      <*> (pure NonModifiable C.<|> pure Modifiable)
                      <*> (pure OtherNoLookupCache C.<|> pure OtherLookupCache)
                      <*> (pure NoLookupCache C.<|> pure LookupCache)
                  )
          )
      <*> ( toKeepType
              <$> ( T3
                      <$> (pure Privileged C.<|> pure NotPrivileged)
                      <*> (pure Secure C.<|> pure NonSecure)
                      <*> (pure Instruction C.<|> pure Data)
                  )
          )
      <*> (toKeepType <$> Gen.enumBounded)
      <*> DfTest.genSmallInt

  genBurstLen = toKeepType <$> pure 0
  genBurst = toKeepType <$> (pure BmFixed C.<|> pure BmIncr C.<|> pure BmWrap)
  genStrobe = genVec $ pure Nothing C.<|> (Just <$> Gen.enumBounded)

prop_axi4_convert_write_id_rev :: Property
prop_axi4_convert_write_id_rev =
  DfTest.idWithModelDf
    defExpectOptions
    (DfTest.genData genInfo)
    id
    ( C.withClockResetEnable @C.System C.clockGen C.resetGen C.enableGen $
        DfConv.dfConvTestBenchRev
          Proxy
          Proxy
          (repeat $ Df.Data fwdInfo)
          (repeat True)
          ckt
    )
 where
  ckt ::
    (C.HiddenClockResetEnable dom) =>
    Circuit
      ( Axi4WriteAddress dom ConfAW Int
      , Axi4WriteData dom ConfW Int
      , Reverse (Axi4WriteResponse dom ConfB Int)
      )
      ( Axi4WriteAddress dom ConfAW Int
      , Axi4WriteData dom ConfW Int
      , Reverse (Axi4WriteResponse dom ConfB Int)
      )
  ckt = DfConv.convert Proxy Proxy

  genInfo = (,) <$> genResp <*> DfTest.genSmallInt
  genResp =
    toKeepType
      <$> ( pure ROkay
              C.<|> pure RExclusiveOkay
              C.<|> pure RSlaveError
              C.<|> pure RDecodeError
          )

  fwdInfo =
    ( Axi4WriteAddressInfo
        { _awiid = 0
        , _awiaddr = 0
        , _awiregion = toKeepType 0
        , _awisize = toKeepType Bs1
        , _awilock = toKeepType NonExclusiveAccess
        , _awicache =
            toKeepType
              ( T4
                  NonBufferable
                  NonModifiable
                  OtherNoLookupCache
                  NoLookupCache
              )
        , _awiprot =
            toKeepType
              ( T3
                  Privileged
                  Secure
                  Instruction
              )
        , _awiqos = toKeepType 0
        , _awiuser = 0
        }
    , toKeepType 0
    , toKeepType BmFixed
    , C.repeat Nothing
    , 0
    )

prop_axi4_convert_read_id :: Property
prop_axi4_convert_read_id =
  DfTest.idWithModelDf
    defExpectOptions
    (DfTest.genData genInfo)
    id
    ( C.withClockResetEnable @C.System C.clockGen C.resetGen C.enableGen $
        DfConv.dfConvTestBench
          Proxy
          Proxy
          (repeat True)
          (repeat $ Df.Data (0, 0, toKeepType ROkay))
          ckt
    )
 where
  ckt ::
    (C.HiddenClockResetEnable dom) =>
    Circuit
      ( Axi4ReadAddress dom ConfAR Int
      , Reverse (Axi4ReadData dom ConfR Int Int)
      )
      ( Axi4ReadAddress dom ConfAR Int
      , Reverse (Axi4ReadData dom ConfR Int Int)
      )
  ckt = DfConv.convert Proxy Proxy

  genInfo =
    Axi4ReadAddressInfo
      <$> Gen.enumBounded
      <*> Gen.enumBounded
      <*> (toKeepType <$> Gen.enumBounded)
      <*> (Gen.integral (Range.linear 0 10))
      <*> ( toKeepType
              <$> ( pure Bs1
                      C.<|> pure Bs2
                      C.<|> pure Bs4
                      C.<|> pure Bs8
                      C.<|> pure Bs16
                      C.<|> pure Bs32
                      C.<|> pure Bs64
                      C.<|> pure Bs128
                  )
          )
      <*> (toKeepType <$> (pure BmFixed C.<|> pure BmIncr C.<|> pure BmWrap))
      <*> (toKeepType <$> (pure NonExclusiveAccess C.<|> pure ExclusiveAccess))
      <*> ( toKeepType
              <$> ( T4
                      <$> (pure NonBufferable C.<|> pure Bufferable)
                      <*> (pure NonModifiable C.<|> pure Modifiable)
                      <*> (pure OtherNoLookupCache C.<|> pure OtherLookupCache)
                      <*> (pure NoLookupCache C.<|> pure LookupCache)
                  )
          )
      <*> ( toKeepType
              <$> ( T3
                      <$> (pure Privileged C.<|> pure NotPrivileged)
                      <*> (pure Secure C.<|> pure NonSecure)
                      <*> (pure Instruction C.<|> pure Data)
                  )
          )
      <*> (toKeepType <$> Gen.enumBounded)
      <*> DfTest.genSmallInt

prop_axi4_convert_read_id_rev :: Property
prop_axi4_convert_read_id_rev =
  DfTest.idWithModelDf
    defExpectOptions
    (DfTest.genData genInfo)
    id
    ( C.withClockResetEnable @C.System C.clockGen C.resetGen C.enableGen $
        DfConv.dfConvTestBenchRev
          Proxy
          Proxy
          (repeat $ Df.Data fwdInfo)
          (repeat True)
          ckt
    )
 where
  ckt ::
    (C.HiddenClockResetEnable dom) =>
    Circuit
      ( Axi4ReadAddress dom ConfAR Int
      , Reverse (Axi4ReadData dom ConfR Int Int)
      )
      ( Axi4ReadAddress dom ConfAR Int
      , Reverse (Axi4ReadData dom ConfR Int Int)
      )
  ckt = DfConv.convert Proxy Proxy

  genInfo =
    (,,)
      <$> DfTest.genSmallInt
      <*> DfTest.genSmallInt
      <*> ( toKeepType
              <$> ( pure ROkay
                      C.<|> pure RExclusiveOkay
                      C.<|> pure RSlaveError
                      C.<|> pure RDecodeError
                  )
          )

  fwdInfo =
    Axi4ReadAddressInfo
      { _ariid = 0
      , _ariaddr = 0
      , _ariregion = toKeepType 0
      , _arilen = toKeepType 0
      , _arisize = toKeepType Bs1
      , _ariburst = toKeepType BmFixed
      , _arilock = toKeepType NonExclusiveAccess
      , _aricache =
          toKeepType
            ( T4
                NonBufferable
                NonModifiable
                OtherNoLookupCache
                NoLookupCache
            )
      , _ariprot =
          toKeepType
            ( T3
                Privileged
                Secure
                Instruction
            )
      , _ariqos = toKeepType 0
      , _ariuser = 0
      }

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
      <$> (genVec Gen.enumBounded)
      <*> (genVec Gen.enumBounded)
      <*> (genVec Gen.enumBounded)
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
