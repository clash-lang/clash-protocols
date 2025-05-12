{-# LANGUAGE NoFieldSelectors #-}

module Protocols.Test.Driver where

import Clash.Signal.Internal (Femtoseconds (..))
import Data.List.Infinite (Infinite)
import Data.Text (Text)
import Protocols.Internal (Protocol (..))
import Protocols.Test.Samples ( Samples )


-- | A 'DriverSamples' is a collection of samples and their timestamps, as well as the
-- status of the driver. It is used to record traces of signals in a simulation.
--
-- TODO: Support different active edges.
data DriverSamples = DriverSamples
  { name :: Text
  -- ^ Name of the driver
  , traces :: [Samples]
  -- ^ (Optionally) traces associated with this driver
  , period :: Maybe Femtoseconds
  -- ^ Period of the clock. Set if dealing with classical clocks (i.e., non-dynamic).
  , statuses :: Infinite (Femtoseconds, Status)
  -- ^ Statuses with their timestamps
  }

instance Protocol DriverSamples where
  type Fwd DriverSamples = DriverSamples
  type Bwd DriverSamples = ()

data Status
  = -- | Do not shut down simulation.
    PreventStop
  | -- | Error condition. Driver is free to go to any other state after
    --  asserting this.
    Error Text
  | -- | Shutting down simulation is OK. Driver may still be monitoring
    -- for errors.
    StopOk
  deriving (Eq, Show)

