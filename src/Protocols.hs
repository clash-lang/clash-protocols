{-|
See 'Circuit' for documentation. This module is designed to import unqualified,
i.e. using:

@
  import Protocols
@

Definitions of 'Circuit', 'Fwd', 'Bwd', 'Protocols.Dfs.Dfs', inspired by
definitions in @circuit-notation@ at <https://github.com/cchalmers/circuit-notation>.
-}

module Protocols
  ( -- * Circuit definition
    Circuit(Circuit)
  , Protocol(Fwd, Bwd)
  , Backpressure(boolsToBwd)
  , Ack(..)

    -- * Combinators & functions
  , (|>), (<|)
  , fromSignals, toSignals

    -- * Protocol types
  , Df
  , Dfs

    -- * Basic circuits
  , idC
  , repeatC
  , prod2C

    -- * Simulation
  , Simulate(SimulateType, SimulateChannels, driveC, sampleC, stallC)
  , SimulationConfig(..)
  , StallAck(..)
  , simulateC

    -- * Circuit notation plugin
  , circuit
  ) where

import Protocols.Internal
import Protocols.Df (Df)
import Protocols.Df.Simple (Dfs)
