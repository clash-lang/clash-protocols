{-|
See 'Circuit' for documentation. This module is designed to import unqualified,
i.e. using:

@
  import Protocols
@

Definitions of 'Circuit', 'Fwd', 'Bwd', 'Protocols.Df.Df', inspired by
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

    -- * Basic circuits
  , idC
  , repeatC
  , prod2C

    -- * Simulation
  , Simulate
     ( SimulateType
     , ExpectType
     , SimulateChannels
     , toSimulateType
     , fromSimulateType
     , driveC
     , sampleC
     , stallC
     )
  , SimulationConfig(..)
  , StallAck(..)
  , simulateC
  , simulateCS
  , def

    -- * Circuit notation plugin
  , circuit, (-<)
  ) where

import Data.Default (def)
import Protocols.Internal
import Protocols.Df (Df)
