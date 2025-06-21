{- |
A GHC source plugin providing a DSL for writing Circuit components. Credits to
@circuit-notation@ at <https://github.com/cchalmers/circuit-notation>.
-}
module Protocols.Plugin (
  -- * Circuit types
  Circuit (..),
  Protocol (..),

  -- * clash-prelude related types
  CSignal,

  -- * plugin functions
  plugin,
  circuit,
  (-<),
) where

-- base
import Prelude

-- clash-protocols
import Protocols.Plugin.Internal
import Protocols.Plugin.TaggedBundle
import Protocols.Plugin.Types
import Protocols.Plugin.Units

-- circuit-notation
import CircuitNotation qualified as CN

-- tagged
import Data.Tagged

-- ghc
import GHC.Plugins qualified as GHC

-- | @circuit-notation@ plugin repurposed for "Protocols".
plugin :: GHC.Plugin
plugin =
  CN.mkPlugin $
    CN.ExternalNames
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
