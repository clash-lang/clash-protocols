{-|
A GHC source plugin providing a DSL for writing Circuit components. Credits to
@circuit-notation@ at <https://github.com/cchalmers/circuit-notation>.
-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE CPP #-}

module Protocols.Plugin where

-- base
import           Prelude

-- clash-protocols
import           Protocols

-- circuit-notation
import qualified CircuitNotation as CN

-- ghc
#if __GLASGOW_HASKELL__ >= 900
import qualified GHC.Plugins as GHC
#else
import qualified GhcPlugins as GHC
#endif

-- | Type inference helper used by circuit-notation plugin
type CircuitT a b = (Fwd a, Bwd b) -> (Bwd a, Fwd b)

-- | 'circuit-notation' plugin repurposed for 'Protocols.protocols'.
plugin :: GHC.Plugin
plugin = CN.mkPlugin $ CN.ExternalNames
  { CN.circuitCon = CN.thName 'Circuit
  , CN.circuitTyCon = CN.thName ''Circuit
  , CN.circuitTTyCon = CN.thName ''CircuitT
  , CN.runCircuitName = CN.thName 'toSignals
  , CN.fwdBwdCon = CN.thName '(,)
  }
