{-# OPTIONS_GHC "-Wno-orphans" #-}

{-|
Functionalities to easily create idle circuits for protocols.
-}
module Protocols.Idle
  ( IdleCircuit(..)
  , idleSource
  , idleSink
  ) where

import Data.Proxy
import Protocols.Cpp (maxTupleSize)
import Protocols.Internal
import Protocols.Internal.Classes
import Protocols.Internal.TH (idleCircuitTupleInstances)

instance (IdleCircuit a, IdleCircuit b) => IdleCircuit (a, b) where
  idleFwd _ = (idleFwd $ Proxy @a, idleFwd $ Proxy @b)
  idleBwd _ = (idleBwd $ Proxy @a, idleBwd $ Proxy @b)

-- Derive instances for tuples up to maxTupleSize
idleCircuitTupleInstances 3 maxTupleSize

-- | Idle state of a source, this circuit does not produce any data.
idleSource :: forall p. (IdleCircuit p) => Circuit () p
idleSource = Circuit $ const ((), idleFwd $ Proxy @p)

-- | Idle state of a sink, this circuit does not consume any data.
idleSink :: forall p. (IdleCircuit p) => Circuit p ()
idleSink = Circuit $ const (idleBwd $ Proxy @p, ())
