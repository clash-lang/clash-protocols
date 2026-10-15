{-# LANGUAGE OverloadedStrings #-}

-- | Tests of the waveforms written by "Protocols.Experimental.Testbench"
module Tests.Protocols.Testbench.Vcd where

import Prelude

import Clash.Prelude (Signal, System)
import Clash.Prelude qualified as C
import Data.Char (isSpace)
import Data.IORef (modifyIORef, newIORef, readIORef)
import Data.Map qualified as Map
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as Text
import Protocols.Experimental.Testbench
import Protocols.Experimental.Testbench.Vcd (identifiers)
import Test.Tasty (TestTree)
import Test.Tasty.HUnit (Assertion, assertBool, testCase, (@?=))
import Test.Tasty.TH (testGroupGenerator)

-- | Simulate a trace tree, returning its waveform
waveform :: TraceTree -> IO (TestResult, Text)
waveform tree = do
  ref <- newIORef []
  r <- simulate (Just (\t -> modifyIORef ref (t :))) Nothing tree
  chunks <- readIORef ref
  pure (r, mconcat (reverse chunks))

goldenTree :: TraceTree
goldenTree =
  traceGroup
    "tb"
    [ traceClock "clk" (C.clockGen @System)
    , traceReset "rst" (C.resetGenN @System C.d2)
    , traceStatus @System "status" (C.fromList_lazy (replicate 4 PreventStop <> repeat StopOk))
    , traceGroup
        "sub group"
        [ traceSignal "count" count
        , cover @System "hit" (C.fromList_lazy ([False, True, True] <> repeat False))
        ]
    ]
 where
  count :: Signal System (C.Unsigned 4)
  count = C.fromList_lazy ([0, 1, C.errorX "undefined", 3] <> repeat 3)

case_golden :: Assertion
case_golden = do
  (r, vcd) <- waveform goldenTree
  r.outcome @?= Passed
  r.endTime @?= cycles @System 4
  Text.lines vcd @?= golden

golden :: [Text]
golden =
  [ "$version clash-protocols testbench $end"
  , "$timescale 1fs $end"
  , "$scope module testbench $end"
  , "  $scope module tb $end"
  , "    $var wire 1 ! clk $end"
  , "    $var wire 1 \" rst $end"
  , "    $var wire 2 # status $end"
  , "    $scope module sub_group $end"
  , "      $var wire 4 $ count $end"
  , "      $var wire 1 % hit $end"
  , "    $upscope $end"
  , "  $upscope $end"
  , "$upscope $end"
  , "$enddefinitions $end"
  , "#0"
  , "$dumpvars"
  , "1!"
  , "1\""
  , "b00 #"
  , "b0000 $"
  , "0%"
  , "$end"
  , "#5000000"
  , "0!"
  , "#10000000"
  , "1!"
  , "b0001 $"
  , "1%"
  , "#15000000"
  , "0!"
  , "#20000000"
  , "1!"
  , "0\""
  , "bxxxx $"
  , "#25000000"
  , "0!"
  , "#30000000"
  , "1!"
  , "b0011 $"
  , "0%"
  , "#35000000"
  , "0!"
  , "#40000000"
  , "1!"
  , "b01 #"
  ]

case_identifiers :: Assertion
case_identifiers = do
  let ids = take 10_000 identifiers
  Set.size (Set.fromList ids) @?= length ids
  assertBool
    "printable"
    (all (Text.all (\c -> c >= '!' && c <= '~' && not (isSpace c))) ids)

case_duplicateNames :: Assertion
case_duplicateNames = do
  let
    hit = C.fromList_lazy ([True, False] <> repeat False)
    tree =
      traceGroup
        "tb"
        [ traceStatus @System "status" (pure StopOk)
        , cover @System "x" hit
        , cover @System "x" hit
        , traceGroup "y" [cover @System "z" hit]
        , traceGroup "y" [cover @System "z" hit]
        ]
  (r, _) <- waveform tree
  Map.keys r.coverage @?= ["tb/x", "tb/x_2", "tb/y/z", "tb/y/z_2"]

tests :: TestTree
tests = $(testGroupGenerator)
