{-# LANGUAGE FlexibleContexts #-}

module Protocols.Internal.Types where

import qualified Clash.Prelude as C
import Data.Default (Default (..))
import Data.Proxy
import GHC.Base (Type)
import Protocols.Plugin

{- $setup
>>> import Protocols
-}

{- | Idle state of a Circuit. Aims to provide no data for both the forward and
backward direction. Transactions are not acknowledged.
-}
class (Protocol p) => IdleCircuit p where
  idleFwd :: Proxy p -> Fwd (p :: Type)
  idleBwd :: Proxy p -> Bwd (p :: Type)

-- | Conversion from booleans to protocol specific acknowledgement values.
class (Protocol a) => Backpressure a where
  -- | Interpret list of booleans as a list of acknowledgements at every cycle.
  -- Implementations don't have to account for finite lists.
  boolsToBwd :: Proxy a -> [Bool] -> Bwd a

{- | Specifies option for simulation functions. Don't use this constructor
directly, as it may be extend with other options in the future. Use 'def'
instead.
-}
data SimulationConfig = SimulationConfig
  { resetCycles :: Int
  -- ^ Assert reset for a number of cycles before driving the protocol
  --
  -- Default: 100
  , timeoutAfter :: Int
  -- ^ Timeout after /n/ cycles. Only affects sample functions.
  --
  -- Default: 'maxBound'
  , ignoreReset :: Bool
  -- ^ Ignore cycles while in reset (sampleC)
  --
  -- Default: False
  }
  deriving (Show)

instance Default SimulationConfig where
  def =
    SimulationConfig
      { resetCycles = 100
      , timeoutAfter = maxBound
      , ignoreReset = False
      }

{- | Determines what kind of acknowledgement signal 'stallC' will send when its
input component is not sending any data. Note that, in the Df protocol,
protocols may send arbitrary acknowledgement signals when this happens.
-}
data StallAck
  = -- | Send Nack
    StallWithNack
  | -- | Send Ack
    StallWithAck
  | -- | Send @errorX "No defined ack"@
    StallWithErrorX
  | -- | Passthrough acknowledgement of RHS component
    StallTransparently
  | -- | Cycle through all modes
    StallCycle
  deriving (Eq, Bounded, Enum, Show)

{- | Class that defines how to /drive/, /sample/, and /stall/ a "Circuit" of
some shape. The "Backpressure" instance requires that the /backward/ type of the
circuit can be generated from a list of Booleans.
-}
class (C.KnownNat (SimulateChannels a), Backpressure a, Simulate a) => Drivable a where
  -- TODO: documentatie verplaatsen
  -- Type a /Circuit/ driver needs or sampler yields. For example:
  --
  -- >>> :kind! (forall dom a. SimulateFwdType (Df dom a))
  -- ...
  -- = [Data a]
  --
  -- This means sampling a @Circuit () (Df dom a)@ with 'sampleC' yields
  -- @[Data a]@.

  -- | Similar to 'SimulateFwdType', but without backpressure information. For
  -- example:
  --
  -- >>> :kind! (forall dom a. ExpectType (Df dom a))
  -- ...
  -- = [a]
  --
  -- Useful in situations where you only care about the "pure functionality" of
  -- a circuit, not its timing information. Leveraged by various functions
  -- in "Protocols.Hedgehog" and 'Protocols.Internal.simulateCS'.
  type ExpectType a :: Type

  -- | Convert a /ExpectType a/, a type representing data without backpressure,
  -- into a type that does, /SimulateFwdType a/.
  toSimulateType ::
    -- | Type witness
    Proxy a ->
    -- | Expect type: input for a protocol /without/ stall information
    ExpectType a ->
    -- | Expect type: input for a protocol /with/ stall information
    SimulateFwdType a

  -- | Convert a /ExpectType a/, a type representing data without backpressure,
  -- into a type that does, /SimulateFwdType a/.
  fromSimulateType ::
    -- | Type witness
    Proxy a ->
    -- | Expect type: input for a protocol /with/ stall information
    SimulateFwdType a ->
    -- | Expect type: input for a protocol /without/ stall information
    ExpectType a

  -- | Create a /driving/ circuit. Can be used in combination with 'sampleC'
  -- to simulate a circuit. Related: 'Protocols.Internal.simulateC'.
  driveC ::
    SimulationConfig ->
    SimulateFwdType a ->
    Circuit () a

  -- | Sample a circuit that is trivially drivable. Use 'driveC'  to create
  -- such a circuit. Related: 'Protocols.Internal.simulateC'.
  sampleC ::
    SimulationConfig ->
    Circuit () a ->
    SimulateFwdType a

{- | Defines functions necessary for implementation of the 'Protocols.Internal.simulateCircuit' function. This
kind of simulation requires a lists for both the forward and the backward direction.

This class requires the definition of the types that the test supplies and returns. Its
functions are converters from these /simulation types/ to types on the 'Clash.Signal.Signal' level.
The 'Protocols.Internal.simulateCircuit' function can thus receive the necessary simulation types, convert
them to types on the 'Clash.Signal.Signal' level, pass those signals to the circuit, and convert the
result of the circuit back to the simulation types giving the final result.
-}
class (C.KnownNat (SimulateChannels a), Protocol a) => Simulate a where
  -- | The type that a test must provide to the 'Protocols.Internal.simulateCircuit' function in the forward direction.
  -- Usually this is some sort of list.
  type SimulateFwdType a :: Type

  -- | The type that a test must provide to the 'Protocols.Internal.simulateCircuit' function in the backward direction.
  -- Usually this is some sort of list
  type SimulateBwdType a :: Type

  -- | The number of simulation channels this channel has after flattening it.
  -- For example, @(Df dom a, Df dom a)@ has 2, while
  -- @Vec 4 (Df dom a, Df dom a)@ has 8.
  type SimulateChannels a :: C.Nat

  -- | Convert the forward simulation type to the 'Fwd' of @a@.
  simToSigFwd :: Proxy a -> SimulateFwdType a -> Fwd a

  -- | Convert the backward simulation type to the 'Bwd' of @a@.
  simToSigBwd :: Proxy a -> SimulateBwdType a -> Bwd a

  -- | Convert a signal of type @Bwd a@ to the backward simulation type.
  sigToSimFwd :: Proxy a -> Fwd a -> SimulateFwdType a

  -- | Convert a signal of type @Fwd a@ to the forward simulation type.
  sigToSimBwd :: Proxy a -> Bwd a -> SimulateBwdType a

  -- | Create a /stalling/ circuit. For each simulation channel (see
  -- 'SimulateChannels') a tuple determines how the component stalls:
  --
  --   * 'StallAck': determines how the backward (acknowledgement) channel
  --     should behave whenever the component does not receive data from the
  --     left hand side or when it's intentionally stalling.
  --
  --   * A list of 'Int's that determine how many stall cycles to insert on
  --     every cycle the left hand side component produces data. I.e., stalls
  --     are /not/ inserted whenever the left hand side does /not/ produce data.
  stallC ::
    SimulationConfig ->
    C.Vec (SimulateChannels a) (StallAck, [Int]) ->
    Circuit a a
