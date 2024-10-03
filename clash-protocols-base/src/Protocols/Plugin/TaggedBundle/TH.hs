{-# OPTIONS_HADDOCK hide #-}

module Protocols.Plugin.TaggedBundle.TH where

import Data.Tagged
import Language.Haskell.TH

appTs :: Q Type -> [Q Type] -> Q Type
appTs = foldl appT

tupT :: [Q Type] -> Q Type
tupT tyArgs = tupleT (length tyArgs) `appTs` tyArgs

taggedBundleTupleInstances :: Int -> Q [Dec]
taggedBundleTupleInstances n = mapM taggedBundleTupleInstance [3 .. n]

taggedBundleTupleInstance :: Int -> Q Dec
taggedBundleTupleInstance n =
  instanceD
    -- No superclasses
    (pure [])
    -- Head
    ( taggedBundleCon
        `appT` (tupleT n `appTs` tagTyVars)
        `appT` (tupleT n `appTs` tyVars)
    )
    -- Implementation
    [ tySynInstD (tySynEqn Nothing aTypeLhs aTypeRhs)
    , funD taggedBundleFunName [clause [bundlePat] (normalB bundleImpl) []]
    , funD taggedUnbundleFunName [clause [unbundlePat] (normalB unbundleImpl) []]
    ]
 where
  -- associated type
  taggedUnbundledCon = conT (mkName "TaggedUnbundled")
  taggedBundleCon = conT (mkName "TaggedBundle")
  aTypeLhs = taggedUnbundledCon `appT` tupT tagTyVars `appT` tupT tyVars
  aTypeRhs = tupT (zipWith mkTaggedTy tagTyVars tyVars)
  mkTaggedTy ta a = conT ''Tagged `appT` ta `appT` a

  -- bundle
  taggedBundleFunName = mkName "taggedBundle"
  bundlePat = tupP (map (conP 'Tagged . pure . varP) varNames)
  bundleImpl = conE 'Tagged `appE` tupE vars

  -- unbundle
  taggedUnbundleFunName = mkName "taggedUnbundle"
  unbundlePat = conP 'Tagged [tupP (map varP varNames)]
  unbundleImpl = tupE [conE 'Tagged `appE` v | v <- vars]

  -- shared
  tagTyVars = map (varT . mkName . ('t' :) . show) [1 .. n]
  tyVars = map varT varNames
  vars = map varE varNames
  varNames = map (mkName . ('a' :) . show) [1 .. n]
