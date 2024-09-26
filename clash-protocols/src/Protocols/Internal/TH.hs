{-# OPTIONS_HADDOCK hide #-}

module Protocols.Internal.TH where

import qualified Clash.Prelude as C
import Control.Monad.Extra (concatMapM)
import GHC.TypeNats
import Language.Haskell.TH
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

      stallC $(varP $ mkName "conf") $(varP $ mkName "rem0") = $(letE (stallVecs ++ stallCircuits) stallCExpr)
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
  stallVecs = zipWith mkStallVec [1 .. n] circTys
  mkStallVec i ty =
    valD
      mkStallPat
      ( normalB [e|(C.splitAtI @(SimulateChannels $ty) $(varE (mkName $ "rem" <> show (i - 1))))|]
      )
      []
   where
    mkStallPat =
      tupP
        [ varP (mkName $ "stalls" <> show i)
        , varP (mkName $ if i == n then "_" else "rem" <> show i)
        ]

  -- stallC Declaration: Generate stalling circuits
  stallCircuits = zipWith mkStallCircuit [1 .. n] circTys
  mkStallCircuit i ty =
    valD
      [p|Circuit $(varP $ mkName $ "stalled" <> show i)|]
      (normalB [e|stallC @($ty) conf $(varE $ mkName $ "stalls" <> show i)|])
      []

  -- Generate the stallC expression
  stallCExpr =
    [e|
      Circuit $ \($fwdPat0, $bwdPat0) -> $(letE stallCResultDecs [e|($(tupE fwdExpr1), $(tupE bwdExpr1))|])
      |]

  stallCResultDecs = map mkStallCResultDec [1 .. n]
  mkStallCResultDec i =
    valD
      (tupP [varP $ mkName $ "fwdStalled" <> show i, varP $ mkName $ "bwdStalled" <> show i])
      ( normalB $
          appE (varE $ mkName $ "stalled" <> show i) $
            tupE [varE $ mkName $ "fwd" <> show i, varE $ mkName $ "bwd" <> show i]
      )
      []
