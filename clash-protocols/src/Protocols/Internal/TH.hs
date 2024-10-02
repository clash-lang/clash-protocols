{-# OPTIONS_HADDOCK hide #-}

module Protocols.Internal.TH where

import Control.Monad.Extra (concatMapM)
import Language.Haskell.TH
import Protocols.Internal.Types

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
