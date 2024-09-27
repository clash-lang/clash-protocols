{-# OPTIONS_HADDOCK hide #-}

module Protocols.Protocol.TH where
import Language.Haskell.TH

appTs :: Q Type -> [Q Type] -> Q Type
appTs = foldl appT

protocolTupleInstances :: Int -> Int -> Q [Dec]
protocolTupleInstances n m = mapM protocolTupleInstance [n .. m]

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
  tyVars = map (varT . mkName . ('a' :) . show) [1 .. n]

  tup = tupleT n `appTs` tyVars

  mkTyInst :: Name -> DecQ
  mkTyInst con =
    tySynInstD $ tySynEqn Nothing lhs rhs
   where
    lhs, rhs :: TypeQ
    lhs = conT con `appT` tup
    rhs = tupleT n `appTs` map (conT con `appT`) tyVars
