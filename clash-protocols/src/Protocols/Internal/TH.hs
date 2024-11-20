{-# OPTIONS_HADDOCK hide #-}

module Protocols.Internal.TH where

import qualified Clash.Prelude as C
import Control.Monad (zipWithM)
import Control.Monad.Extra (concatMapM)
import Data.Proxy
import GHC.TypeNats
import Language.Haskell.TH
import Protocols.Hedgehog.Types
import Protocols.Internal.Types
import Protocols.Plugin

{- | Template haskell function to generate IdleCircuit instances for the tuples
n through m inclusive. To see a 2-tuple version of the pattern we generate,
see @Protocols.IdleCircuit@.
-}
idleCircuitTupleInstances :: Int -> Int -> DecsQ
idleCircuitTupleInstances n m = concatMapM idleCircuitTupleInstance [n .. m]

{- | Template Haskell function to generate an IdleCircuit instance for an
n-tuple.
-}
idleCircuitTupleInstance :: Int -> DecsQ
idleCircuitTupleInstance n =
  [d|
    instance ($instCtx) => IdleCircuit $instTy where
      idleFwd _ = $fwdExpr
      idleBwd _ = $bwdExpr
    |]
 where
  circTys = map (\i -> varT $ mkName $ "c" <> show i) [1 .. n]
  instCtx = foldl appT (tupleT n) $ map (\ty -> [t|IdleCircuit $ty|]) circTys
  instTy = foldl appT (tupleT n) circTys
  fwdExpr = tupE $ map mkFwdExpr circTys
  mkFwdExpr ty = [e|idleFwd $ Proxy @($ty)|]
  bwdExpr = tupE $ map mkBwdExpr circTys
  mkBwdExpr ty = [e|idleBwd $ Proxy @($ty)|]

simulateTupleInstances :: Int -> Int -> DecsQ
simulateTupleInstances n m = concatMapM simulateTupleInstance [n .. m]

simulateTupleInstance :: Int -> DecsQ
simulateTupleInstance n =
  [d|
    instance ($instCtx) => Simulate $instTy where
      type SimulateFwdType $instTy = $fwdType
      type SimulateBwdType $instTy = $bwdType
      type SimulateChannels $instTy = $channelSum

      simToSigFwd _ $fwdPat0 = $(tupE $ zipWith (\ty expr -> [e|simToSigFwd (Proxy @($ty)) $expr|]) circTys fwdExpr)
      simToSigBwd _ $bwdPat0 = $(tupE $ zipWith (\ty expr -> [e|simToSigBwd (Proxy @($ty)) $expr|]) circTys bwdExpr)
      sigToSimFwd _ $fwdPat0 = $(tupE $ zipWith (\ty expr -> [e|sigToSimFwd (Proxy @($ty)) $expr|]) circTys fwdExpr)
      sigToSimBwd _ $bwdPat0 = $(tupE $ zipWith (\ty expr -> [e|sigToSimBwd (Proxy @($ty)) $expr|]) circTys bwdExpr)

      stallC $(varP $ mkName "conf") $(varP $ mkName "rem0") = $stallCExpr
    |]
 where
  -- Generate the types for the instance
  circTys = map (\i -> varT $ mkName $ "c" <> show i) [1 .. n]
  instTy = foldl appT (tupleT n) circTys
  instCtx = foldl appT (tupleT n) $ map (\ty -> [t|Simulate $ty|]) circTys
  fwdType = foldl appT (tupleT n) $ map (\ty -> [t|SimulateFwdType $ty|]) circTys
  bwdType = foldl appT (tupleT n) $ map (\ty -> [t|SimulateBwdType $ty|]) circTys
  channelSum = foldl1 (\a b -> [t|$a + $b|]) $ map (\ty -> [t|SimulateChannels $ty|]) circTys

  -- Relevant expressions and patterns
  fwdPat0 = tupP $ map (\i -> varP $ mkName $ "fwd" <> show i) [1 .. n]
  bwdPat0 = tupP $ map (\i -> varP $ mkName $ "bwd" <> show i) [1 .. n]
  fwdExpr = map (\i -> varE $ mkName $ "fwd" <> show i) [1 .. n]
  bwdExpr = map (\i -> varE $ mkName $ "bwd" <> show i) [1 .. n]
  fwdExpr1 = map (\i -> varE $ mkName $ "fwdStalled" <> show i) [1 .. n]
  bwdExpr1 = map (\i -> varE $ mkName $ "bwdStalled" <> show i) [1 .. n]

  -- stallC Declaration: Split off the stall vectors from the large input vector
  mkStallVec i ty =
    [d|
      $[p|
        ( $(varP (mkName $ "stalls" <> show i))
          , $(varP (mkName $ if i == n then "_" else "rem" <> show i))
          )
        |] =
          C.splitAtI @(SimulateChannels $ty)
            $(varE $ mkName $ "rem" <> show (i - 1))
      |]

  -- stallC Declaration: Generate stalling circuits
  mkStallCircuit i ty =
    [d|
      $[p|Circuit $(varP $ mkName $ "stalled" <> show i)|] =
        stallC @($ty) conf $(varE $ mkName $ "stalls" <> show i)
      |]

  -- Generate the stallC expression
  stallCExpr = do
    stallVecs <-
      concat <$> zipWithM mkStallVec [1 .. n] circTys
    stallCircuits <-
      concat <$> zipWithM mkStallCircuit [1 .. n] circTys
    LetE (stallVecs <> stallCircuits)
      <$> [e|Circuit $ \($fwdPat0, $bwdPat0) -> $circuitResExpr|]

  circuitResExpr = do
    stallCResultDecs <- concatMapM mkStallCResultDec [1 .. n]
    LetE stallCResultDecs <$> [e|($(tupE fwdExpr1), $(tupE bwdExpr1))|]

  mkStallCResultDec i =
    [d|
      $[p|
        ( $(varP $ mkName $ "fwdStalled" <> show i)
          , $(varP $ mkName $ "bwdStalled" <> show i)
          )
        |] =
          $(varE $ mkName $ "stalled" <> show i)
            ( $(varE $ mkName $ "fwd" <> show i)
            , $(varE $ mkName $ "bwd" <> show i)
            )
      |]

drivableTupleInstances :: Int -> Int -> DecsQ
drivableTupleInstances n m = concatMapM drivableTupleInstance [n .. m]

drivableTupleInstance :: Int -> DecsQ
drivableTupleInstance n =
  [d|
    instance ($instCtx) => Drivable $instTy where
      type
        ExpectType $instTy =
          $(foldl appT (tupleT n) $ map (\ty -> [t|ExpectType $ty|]) circTys)
      toSimulateType Proxy $(tupP circPats) = $toSimulateExpr

      fromSimulateType Proxy $(tupP circPats) = $fromSimulateExpr

      driveC $(varP $ mkName "conf") $(tupP fwdPats) = $(letE driveCDecs driveCExpr)
      sampleC conf (Circuit f) =
        let
          $(varP $ mkName "bools") = replicate (resetCycles conf) False <> repeat True
          $(tupP fwdPats) = snd $ f ((), $(tupE $ map mkSampleCExpr circTys))
         in
          $( tupE $
              zipWith (\ty fwd -> [|sampleC @($ty) conf (Circuit $ const ((), $fwd))|]) circTys fwdExprs
           )
    |]
 where
  circStrings = map (\i -> "c" <> show i) [1 .. n]
  circTys = map (varT . mkName) circStrings
  circPats = map (varP . mkName) circStrings
  circExprs = map (varE . mkName) circStrings
  instCtx = foldl appT (tupleT n) $ map (\ty -> [t|Drivable $ty|]) circTys
  instTy = foldl appT (tupleT n) circTys
  fwdPats = map (varP . mkName . ("fwd" <>)) circStrings
  fwdExprs = map (varE . mkName . ("fwd" <>)) circStrings
  bwdExprs = map (varE . mkName . ("bwd" <>)) circStrings
  bwdPats = map (varP . mkName . ("bwd" <>)) circStrings

  mkSampleCExpr ty = [e|boolsToBwd (Proxy @($ty)) bools|]
  driveCDecs =
    pure $
      valD
        (tupP $ map (\p -> [p|(Circuit $p)|]) circPats)
        (normalB $ tupE $ zipWith (\ty fwd -> [e|driveC @($ty) conf $fwd|]) circTys fwdExprs)
        []

  driveCExpr =
    [e|
      Circuit $ \(_, $(tildeP $ tupP bwdPats)) -> ((), $(tupE $ zipWith mkDriveCExpr circExprs bwdExprs))
      |]
  mkDriveCExpr c bwd = [e|snd ($c ((), $bwd))|]
  toSimulateExpr = tupE $ zipWith (\ty c -> [|toSimulateType (Proxy @($ty)) $c|]) circTys circExprs
  fromSimulateExpr = tupE $ zipWith (\ty c -> [|fromSimulateType (Proxy @($ty)) $c|]) circTys circExprs

backPressureTupleInstances :: Int -> Int -> DecsQ
backPressureTupleInstances n m = concatMapM backPressureTupleInstance [n .. m]

backPressureTupleInstance :: Int -> DecsQ
backPressureTupleInstance n =
  [d|
    instance ($instCtx) => Backpressure $instTy where
      boolsToBwd _ bs = $(tupE $ map (\ty -> [e|boolsToBwd (Proxy @($ty)) bs|]) circTys)
    |]
 where
  circTys = map (\i -> varT $ mkName $ "c" <> show i) [1 .. n]
  instCtx = foldl appT (tupleT n) $ map (\ty -> [t|Backpressure $ty|]) circTys
  instTy = foldl appT (tupleT n) circTys

testTupleInstances :: Int -> Int -> DecsQ
testTupleInstances n m = concatMapM testTupleInstance [n .. m]

testTupleInstance :: Int -> DecsQ
testTupleInstance n =
  [d|
    instance ($instCtx) => Test $instTy where
      expectN Proxy $(varP $ mkName "opts") $(tupP sampledPats) = $(doE stmts)
    |]
 where
  circStrings = map (\i -> "c" <> show i) [1 .. n]
  circTys = map (varT . mkName) circStrings
  instCtx = foldl appT (tupleT n) $ map (\ty -> [t|Test $ty|]) circTys
  instTy = foldl appT (tupleT n) circTys

  sampledPats = map (varP . mkName . ("sampled" <>)) circStrings
  sampledExprs = map (varE . mkName . ("sampled" <>)) circStrings
  trimmedPats = map (varP . mkName . ("trimmed" <>)) circStrings
  trimmedExprs = map (varE . mkName . ("trimmed" <>)) circStrings

  mkTrimStmt trim ty sam = bindS trim [e|expectN (Proxy @($ty)) opts $sam|]
  expectResult = noBindS [e|pure $(tupE trimmedExprs)|]
  stmts = zipWith3 mkTrimStmt trimmedPats circTys sampledExprs <> [expectResult]
