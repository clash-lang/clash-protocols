{-# OPTIONS_GHC "-Wno-orphans" #-}

{- |
Functionalities to easily create idle circuits for protocols.
-}
module Protocols.Idle (
  -- * Type classes
  IdleCircuit (..),

  -- * Utility functions
  idleSource,
  idleSink,
  forceResetSanityGeneric,
) where

import Clash.Prelude
import Prelude ()

import Data.Proxy
import Protocols.Internal
import Protocols.Internal.TH (idleCircuitTupleInstances)
import Protocols.Plugin.Cpp (maxTupleSize)

instance (IdleCircuit a, IdleCircuit b) => IdleCircuit (a, b) where
  idleFwd _ = (idleFwd $ Proxy @a, idleFwd $ Proxy @b)
  idleBwd _ = (idleBwd $ Proxy @a, idleBwd $ Proxy @b)

instance (IdleCircuit a, KnownNat n) => IdleCircuit (Vec n a) where
  idleFwd _ = repeat $ idleFwd $ Proxy @a
  idleBwd _ = repeat $ idleBwd $ Proxy @a

instance IdleCircuit () where
  idleFwd _ = ()
  idleBwd _ = ()

-- Derive instances for tuples up to maxTupleSize
idleCircuitTupleInstances 3 maxTupleSize

-- | Idle state of a source, this circuit does not produce any data.
idleSource :: forall p. (IdleCircuit p) => Circuit () p
idleSource = Circuit $ const ((), idleFwd $ Proxy @p)

-- | Idle state of a sink, this circuit does not consume any data.
idleSink :: forall p. (IdleCircuit p) => Circuit p ()
idleSink = Circuit $ const (idleBwd $ Proxy @p, ())

{- | Force a /nack/ on the backward channel and /no data/ on the forward
channel if reset is asserted.
-}
forceResetSanityGeneric ::
  forall dom a fwd bwd.
  ( KnownDomain dom
  , HiddenReset dom
  , IdleCircuit a
  , Fwd a ~ Signal dom fwd
  , Bwd a ~ Signal dom bwd
  ) =>
  Circuit a a
forceResetSanityGeneric = Circuit go
 where
  go (fwd, bwd) =
    unbundle
      $ mux
        rstAsserted
        (bundle (idleBwd $ Proxy @a, idleFwd $ Proxy @a))
        (bundle (bwd, fwd))

#if MIN_VERSION_clash_prelude(1,8,0)
  rstAsserted = unsafeToActiveHigh hasReset
#else
  rstAsserted = unsafeToHighPolarity hasReset
#endif
