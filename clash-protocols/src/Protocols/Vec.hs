module Protocols.Vec where

-- base
import Prelude hiding (const, map)

-- clash-prelude
import qualified Clash.Prelude as C

-- clash-protocols-base
import Protocols.Plugin

{- | "Bundle" together a 'C.Vec' of 'Circuit's into a 'Circuit' with 'C.Vec' input and output.
The 'Circuit's all run in parallel.
-}
vecCircuits :: (C.KnownNat n) => C.Vec n (Circuit a b) -> Circuit (C.Vec n a) (C.Vec n b)
vecCircuits fs = Circuit (\inps -> C.unzip $ f <$> fs <*> uncurry C.zip inps)
 where
  f (Circuit ff) = ff
