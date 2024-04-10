{-# OPTIONS_GHC "-Wno-orphans" #-}

module Protocols.IdleCircuit
  ( IdleData(..)
  , IdleCircuit(..)
  ) where

import GHC.TypeNats
import Protocols.Cpp (maxTupleSize)
import Protocols.Internal
import Protocols.Internal.Classes (IdleCircuit(..))
import Protocols.Internal.TH (idleCircuitTupleInstances)

import qualified Clash.Sized.Vector as Vec

-- | Idle state of a data type.
class IdleData p where
  idleData :: p

instance IdleData () where
  idleData = ()

instance (IdleData a) => IdleCircuit (CSignal dom a) where
  idleSource = Circuit $ const ((), pure idleData)
  idleSink = Circuit $ const (pure (), ())

instance (IdleCircuit a, IdleCircuit b) => IdleCircuit (a, b) where
  idleSource = Circuit go
   where
    go (_,(aBwd, bBwd)) =
      let
        (_, aFwd) = toSignals (idleSource @a) ((), aBwd)
        (_, bFwd) = toSignals (idleSource @b) ((), bBwd)
      in ((), (aFwd, bFwd))

  idleSink = Circuit go
   where
    go ((aFwd, bFwd),_) =
      let
        (aBwd, _) = toSignals (idleSink @a) (aFwd, ())
        (bBwd, _) = toSignals (idleSink @b) (bFwd, ())
      in ((aBwd, bBwd), ())

-- Derive instances for tuples up to maxTupleSize
idleCircuitTupleInstances 3 maxTupleSize

instance (IdleCircuit p, KnownNat n) => IdleCircuit (Vec.Vec n p) where
  idleSource = Circuit go
   where
    go ((), bwds) = ((), fwds)
     where
      (_,fwds) = Vec.unzip $ fmap (toSignals (idleSource @p)) (Vec.zip (Vec.repeat ()) bwds)

  idleSink = Circuit go
   where
     go (fwds, _) = (bwds, ())
      where
       (bwds,_) = Vec.unzip $ fmap (toSignals (idleSink @p)) (Vec.zip fwds (Vec.repeat ()))
