{-# OPTIONS_GHC -fno-warn-orphans #-}

{- |
A GHC source plugin providing a DSL for writing Circuit components. Credits to
@circuit-notation@ at <https://github.com/cchalmers/circuit-notation>.
-}
module Protocols.Plugin (
  -- * Circuit types
  Circuit (..),
  Protocol (..),
  ToConst,
  ToConstBwd,

  -- * clash-prelude related types
  CSignal,

  -- * plugin functions
  plugin,
  circuit,
  (-<),
) where

-- base

import Data.Kind (Type)
import Prelude

-- clash-prelude
import Clash.Explicit.Prelude qualified as C

-- clash-protocols
import Protocols.Plugin.Cpp
import Protocols.Plugin.Internal
import Protocols.Plugin.TH
import Protocols.Plugin.TaggedBundle
import Protocols.Plugin.Types
import Protocols.Plugin.Units

-- circuit-notation
import CircuitNotation qualified as CN

-- tagged
import Data.Tagged

-- ghc
import GHC.Plugins qualified as GHC

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

{- | A protocol that carries a constant value in the forward direction and no
information in the backward direction.
-}
data ToConst (a :: Type)

instance Protocol (ToConst a) where
  type Fwd (ToConst a) = a
  type Bwd (ToConst a) = ()

{- | A protocol that carries no information in the forward direction and a
constant value in the backward direction.
-}
data ToConstBwd (a :: Type)

instance Protocol (ToConstBwd a) where
  type Fwd (ToConstBwd a) = ()
  type Bwd (ToConstBwd a) = a

instance (C.KnownNat n) => Protocol (C.Vec n a) where
  type Fwd (C.Vec n a) = C.Vec n (Fwd a)
  type Bwd (C.Vec n a) = C.Vec n (Bwd a)

-- XXX: Type families with Signals on LHS are currently broken on Clash:
instance Protocol (CSignal dom a) where
  type Fwd (CSignal dom a) = C.Signal dom a
  type Bwd (CSignal dom a) = ()

instance Protocol (C.Clock dom) where
  type Fwd (C.Clock dom) = C.Clock dom
  type Bwd (C.Clock dom) = ()

instance Protocol (C.DiffClock dom) where
  type Fwd (C.DiffClock dom) = C.DiffClock dom
  type Bwd (C.DiffClock dom) = ()

instance Protocol (C.Reset dom) where
  type Fwd (C.Reset dom) = C.Reset dom
  type Bwd (C.Reset dom) = ()

instance Protocol (C.Enable dom) where
  type Fwd (C.Enable dom) = C.Enable dom
  type Bwd (C.Enable dom) = ()

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
