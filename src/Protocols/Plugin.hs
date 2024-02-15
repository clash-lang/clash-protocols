{-|
A GHC source plugin providing a DSL for writing Circuit components. Credits to
@circuit-notation@ at <https://github.com/cchalmers/circuit-notation>.
-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}

module Protocols.Plugin
  ( plugin
  , circuit
  , (-<)
  ) where

-- base
import           Prelude

-- clash-protocols
import           Protocols
import           Protocols.Plugin.Internal

-- circuit-notation
import qualified CircuitNotation as CN

-- tagged
import Data.Tagged

-- ghc
#if __GLASGOW_HASKELL__ >= 900
import qualified GHC.Plugins as GHC
#else
import qualified GhcPlugins as GHC
#endif

-- | @circuit-notation@ plugin repurposed for "Protocols".
plugin :: GHC.Plugin
plugin = CN.mkPlugin $ CN.ExternalNames
  { CN.circuitCon = CN.thName 'TaggedCircuit
  , CN.fwdAndBwdTypes = \case
      CN.Fwd -> CN.thName ''Fwd
      CN.Bwd -> CN.thName ''Bwd
  , CN.fwdBwdCon = CN.thName '(,)
  , CN.runCircuitName = CN.thName 'taggedCircuit
  , CN.tagBundlePat = CN.thName 'TaggedBundle
  , CN.tagName = CN.thName 'Tagged
  , CN.tagTName = CN.thName ''Tagged
  , CN.trivialBwd = CN.thName 'units
  , CN.consPat = CN.thName '(:>!)
  }
