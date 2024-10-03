{-# OPTIONS_GHC -fno-warn-orphans #-}

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

-- clash-prelude
import qualified Clash.Explicit.Prelude as C

-- clash-protocols
import Protocols.Plugin.Cpp
import Protocols.Plugin.Internal
import Protocols.Plugin.TH
import Protocols.Plugin.TaggedBundle
import Protocols.Plugin.Types
import Protocols.Plugin.Units

-- circuit-notation
import qualified CircuitNotation as CN

-- tagged
import Data.Tagged

-- ghc
import qualified GHC.Plugins as GHC

instance Protocol () where
  type Fwd () = ()
  type Bwd () = ()

{- | __NB__: The documentation only shows instances up to /3/-tuples. By
default, instances up to and including /12/-tuples will exist. If the flag
@large-tuples@ is set instances up to the GHC imposed limit will exist. The
GHC imposed limit is either 62 or 64 depending on the GHC version.
-}
instance Protocol (a, b) where
  type Fwd (a, b) = (Fwd a, Fwd b)
  type Bwd (a, b) = (Bwd a, Bwd b)

-- Generate n-tuple instances, where n > 2
protocolTupleInstances 3 maxTupleSize

instance (C.KnownNat n) => Protocol (C.Vec n a) where
  type Fwd (C.Vec n a) = C.Vec n (Fwd a)
  type Bwd (C.Vec n a) = C.Vec n (Bwd a)

-- XXX: Type families with Signals on LHS are currently broken on Clash:
instance Protocol (CSignal dom a) where
  type Fwd (CSignal dom a) = C.Signal dom a
  type Bwd (CSignal dom a) = C.Signal dom ()

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
