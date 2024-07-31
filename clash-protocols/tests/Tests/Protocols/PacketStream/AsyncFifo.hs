{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Tests.Protocols.PacketStream.AsyncFifo where

-- base
import Prelude

-- clash-prelude
import Clash.Prelude as C

-- hedgehog
import Hedgehog
import qualified Hedgehog.Range as Range

-- tasty
import Test.Tasty
import Test.Tasty.Hedgehog (HedgehogTestLimit (HedgehogTestLimit))
import Test.Tasty.Hedgehog.Extra (testProperty)
import Test.Tasty.TH (testGroupGenerator)

-- clash-protocols
import Protocols.Hedgehog
import Protocols.PacketStream.AsyncFifo

-- tests
import Tests.Protocols.PacketStream.Base

createDomain
  vSystem
    { vName = "TestDom50"
    , vPeriod = 20_000
    , vActiveEdge = Rising
    , vResetKind = Asynchronous
    , vInitBehavior = Unknown
    , vResetPolarity = ActiveHigh
    }

createDomain
  vSystem
    { vName = "TestDom125"
    , vPeriod = 8_000
    , vActiveEdge = Rising
    , vResetKind = Asynchronous
    , vInitBehavior = Unknown
    , vResetPolarity = ActiveHigh
    }

clk50 :: Clock TestDom50
clk50 = clockGen

clk125 :: Clock TestDom125
clk125 = clockGen

-- Assert the reset for a different amount of cycles in each domain
-- to properly test the async fifo.
rst50 :: Reset TestDom50
rst50 = resetGenN d50

rst125 :: Reset TestDom125
rst125 = resetGenN d60

en50 :: Enable TestDom50
en50 = enableGen

en125 :: Enable TestDom125
en125 = enableGen

generateAsyncFifoIdProp ::
  forall (wDom :: Domain) (rDom :: Domain).
  (KnownDomain wDom, KnownDomain rDom) =>
  Clock wDom ->
  Reset wDom ->
  Enable wDom ->
  Clock rDom ->
  Reset rDom ->
  Enable rDom ->
  Property
generateAsyncFifoIdProp wClk wRst wEn rClk rRst rEn =
  idWithModel
    defExpectOptions
    (genValidPackets (Range.linear 1 10) (Range.linear 1 30) Abort)
    id
    (asyncFifoC @wDom @rDom @4 @1 @Int d4 wClk wRst wEn rClk rRst rEn)

{- | The async FIFO circuit should forward all of its input data without loss and without producing extra data.
  This property tests whether this is true, when the clock of the writer and reader is equally fast (50 MHz).
-}
prop_asyncfifo_writer_speed_equal_to_reader_id :: Property
prop_asyncfifo_writer_speed_equal_to_reader_id = generateAsyncFifoIdProp clk50 rst50 en50 clk50 rst50 en50

{- | The async FIFO circuit should forward all of its input data without loss and without producing extra data.
  This property tests whether this is true, when the clock of the writer (50 MHz) is slower than the clock of the reader (125 MHz).
-}
prop_asyncfifo_writer_speed_slower_than_reader_id :: Property
prop_asyncfifo_writer_speed_slower_than_reader_id = generateAsyncFifoIdProp clk50 rst50 en50 clk125 rst125 en125

{- | The async FIFO circuit should forward all of its input data without loss and without producing extra data.
  This property tests whether this is true, when the clock of the writer (125 MHz) is faster than the clock of the reader (50 MHz).
-}
prop_asyncfifo_writer_speed_faster_than_reader_id :: Property
prop_asyncfifo_writer_speed_faster_than_reader_id = generateAsyncFifoIdProp clk125 rst125 en125 clk50 rst50 en50

tests :: TestTree
tests =
  localOption (mkTimeout 20_000_000 {- 20 seconds -}) $
    localOption
      (HedgehogTestLimit (Just 100))
      $(testGroupGenerator)
