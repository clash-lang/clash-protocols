{-# OPTIONS_HADDOCK hide #-}

module Protocols.Plugin.Units.TH (unitsTupleInstances) where

import Language.Haskell.TH

appTs :: Q Type -> [Q Type] -> Q Type
appTs = foldl appT

unitsTupleInstances :: Int -> Q [Dec]
unitsTupleInstances n = mapM unitsTupleInstance [3 .. n]

unitsTupleInstance :: Int -> Q Dec
unitsTupleInstance n =
  instanceD
    (mapM (\v -> unitsConT `appT` v) tyVars) -- context
    (unitsConT `appT` (tupleT n `appTs` tyVars)) -- head
    [funD unitsFunName [clause [] (normalB (tupE [unitsFun | _ <- tyVars])) []]] -- impl
 where
  unitsFun = varE unitsFunName
  unitsFunName = mkName "units"
  unitsConT = conT (mkName "Units")
  tyVars = map (varT . mkName . ('a' :) . show) [1 .. n]
