{-# OPTIONS_HADDOCK hide #-}

module Protocols.Internal.TH where

import Control.Monad.Extra (concatMapM)
import Language.Haskell.TH

import Protocols.Internal.Classes

appTs :: Q Type -> [Q Type] -> Q Type
appTs = foldl appT

protocolTupleInstances :: Int -> Int -> Q [Dec]
protocolTupleInstances n m = mapM protocolTupleInstance [n..m]

protocolTupleInstance :: Int -> Q Dec
protocolTupleInstance n =
  instanceD
    (pure []) -- context
    (protocolConT `appT` tup) -- head
    [mkTyInst fwdConName, mkTyInst bwdConName] -- body

 where
  fwdConName = mkName "Fwd"
  bwdConName = mkName "Bwd"
  protocolConT = conT (mkName "Protocol")

  tyVars :: [TypeQ]
  tyVars = map (varT . mkName . ('a':) . show) [1..n]

  tup = tupleT n `appTs` tyVars

  mkTyInst :: Name -> DecQ
  mkTyInst con =
      tySynInstD $ tySynEqn Nothing lhs rhs
    where
      lhs, rhs :: TypeQ
      lhs = conT con `appT` tup
      rhs = tupleT n `appTs` map (conT con `appT`) tyVars

-- | Template haskell function to generate IdleCircuit instances for the tuples
-- n through m inclusive. To see a 2-tuple version of the pattern we generate,
-- see @Protocols.IdleCircuit@.
idleCircuitTupleInstances :: Int -> Int -> DecsQ
idleCircuitTupleInstances n m = concatMapM idleCircuitTupleInstance [n..m]

-- | Template Haskell function to generate an IdleCircuit instance for an
-- n-tuple.
idleCircuitTupleInstance :: Int -> DecsQ
idleCircuitTupleInstance n =
  [d| instance $instCtx => IdleCircuit $instTy where
        idleSource = Circuit go
         where
          go (_, $srcPat) = $srcExp

        idleSink = Circuit go
         where
          go ($snkPat, _) = $snkExp
    |]
 where
  circTys = map (\i -> varT $ mkName $ "c" <> show i) [1..n]
  instCtx = foldl appT (tupleT n) $ map (\ty -> [t| IdleCircuit $ty |]) circTys
  instTy = foldl appT (tupleT n) circTys
  fwdNms = map (\i -> mkName $ "fwd" <> show i) [1..n]
  bwdNms = map (\i -> mkName $ "bwd" <> show i) [1..n]
  srcPat = tupP $ map varP bwdNms
  srcTup = tupE $ map varE fwdNms
  srcDec (fwdNm, bwdNm, ty) =
    [d| (_, $(varP fwdNm)) = toSignals (idleSource @($ty)) ((), $(varE bwdNm)) |]
  srcDecs = concatMapM srcDec $ zip3 fwdNms bwdNms circTys
  srcExp = liftA2 LetE srcDecs [| ((), $srcTup) |]
  snkPat = tupP $ map varP fwdNms
  snkTup = tupE $ map varE bwdNms
  snkDec (bwdNm, fwdNm, ty) =
    [d| ($(varP bwdNm), _) = toSignals (idleSink @($ty)) ($(varE fwdNm), ()) |]
  snkDecs = concatMapM snkDec $ zip3 bwdNms fwdNms circTys
  snkExp = liftA2 LetE snkDecs [| ($snkTup, ()) |]
