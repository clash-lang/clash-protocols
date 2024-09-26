{-# LANGUAGE FlexibleContexts #-}

{- |
These class definitions are needed to be able to write Template Haskell quotes
for instances. They are defined separately to avoid import loops.

This module is not exported; the classes and their (orphan) instances are
exported elsewhere.
-}
module Protocols.Internal.Classes where

import qualified Clash.Prelude as C
import Clash.Signal
import Data.Default (Default (def))
import Data.Kind (Type)
import Data.Proxy

-- | A protocol describes the in- and outputs of one side of a 'Circuit'.
class Protocol a where
  -- | Sender to receiver type family. See 'Circuit' for an explanation on the
  -- existence of 'Fwd'.
  type Fwd (a :: Type)

  -- | Receiver to sender type family. See 'Circuit' for an explanation on the
  -- existence of 'Bwd'.
  type Bwd (a :: Type)

{- | A /Circuit/, in its most general form, corresponds to a component with two
pairs of an input and output. As a diagram:

@
            Circuit a b

           +-----------+
    Fwd a  |           |  Fwd b
  +------->+           +-------->
           |           |
           |           |
    Bwd a  |           |  Bwd b
  <--------+           +<-------+
           |           |
           +-----------+
@

The first pair, @(Fwd a, Bwd a)@ can be thought of the data sent to and from
the component on the left hand side of this circuit. For this pair, @Fwd a@
is the data sent from the circuit on the left hand side (not pictured), while
@Bwd a@ is the data sent to the left hand side from the current circuit.

Similarly, the second pair, @(Fwd b, Bwd)@, can be thought of as the data
sent to and from the right hand side of this circuit. In this case, @Fwd b@
is the data sent from the current circuit to the one on the right hand side,
while @Bwd b@ is the data received from the right hand side.

In Haskell terms, we would say this is simply a function taking two inputs,
@Fwd a@ and @Bwd b@, yielding a pair of outputs @Fwd b@ and @Bwd a@. This is
in fact exactly its definition:

@
  newtype Circuit a b =
    Circuit ( (Fwd a, Bwd b) -> (Bwd a, Fwd b) )
@

Note that the type parameters /a/ and /b/ don't directly correspond to the
types of the inputs and outputs of this function. Instead, the type families
@Fwd@ and @Bwd@ decide this. The type parameters can be thought of as
deciders for what /protocol/ the left hand side and right hand side must
speak.

Let's make it a bit more concrete by building such a protocol. For this
example, we'd like to build a protocol that sends data to a circuit, while
allowing the circuit to signal whether it processed the sent data or not. Similarly,
we'd like the sender to be able to indicate that it doesn't have any data to
send. These kind of protocols fall under the umbrella of "dataflow" protocols,
so lets call it /DataFlowSimple/ or /Df/ for short:

@
  data Df (dom :: Domain) (a :: Type)
@

We're only going to use it on the type level, so we won't need any
constructors for this datatype. The first type parameter indicates the
synthesis domain the protocol will use. This is the same /dom/ as used in
/Signal dom a/. The second type indicates what data the protocol needs to
send. Again, this is similar to the /a/ in /Signal dom a/.

As said previously, we'd like the sender to either send /no data/ or
/some data/. We can capture this in a data type very similar to /Maybe/:

@
  data Data a = NoData | Data a
@

On the way back, we'd like to either acknowledge or not acknowledge sent
data. Similar to /Bool/ we define:

@
  newtype Ack = Ack Bool
@

With these three definitions we're ready to make an instance for /Fwd/ and
/Bwd/:

@
instance Protocol (Df dom a) where
  type Fwd (Df dom a) = Signal dom (Data a)
  type Bwd (Df dom a) = Signal dom Ack
@

Having defined all this, we can take a look at /Circuit/ once more: now
instantiated with our types. The following:

@
  f :: Circuit (Df dom a) (Df dom b)
@

..now corresponds to the following protocol:

@
                           +-----------+
      Signal dom (Data a)  |           |  Signal dom (Data b)
 +------------------------>+           +------------------------->
                           |           |
                           |           |
      Signal dom Ack       |           |  Signal dom Ack
 <-------------------------+           +<------------------------+
                           |           |
                           +-----------+
@

There's a number of advantages over manually writing out these function
types:

  1. It reduces syntactical noise in type signatures

  2. It eliminates the need for manually routing acknowledgement lines
-}
newtype Circuit a b
  = Circuit ((Fwd a, Bwd b) -> (Bwd a, Fwd b))

-- | Conversion from booleans to protocol specific acknowledgement values.
class (Protocol a) => Backpressure a where
  -- | Interpret list of booleans as a list of acknowledgements at every cycle.
  -- Implementations don't have to account for finite lists.
  boolsToBwd :: Proxy a -> [Bool] -> Bwd a

--------------------------------- SIMULATION -----------------------------------

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
  -- in "Protocols.Hedgehog" and 'simulateCS'.
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
  -- to simulate a circuit. Related: 'simulateC'.
  driveC ::
    SimulationConfig ->
    SimulateFwdType a ->
    Circuit () a

  -- | Sample a circuit that is trivially drivable. Use 'driveC'  to create
  -- such a circuit. Related: 'simulateC'.
  sampleC ::
    SimulationConfig ->
    Circuit () a ->
    SimulateFwdType a

{- | Defines functions necessary for implementation of the 'simulateCircuit' function. This
kind of simulation requires a lists for both the forward and the backward direction.

This class requires the definition of the types that the test supplies and returns. Its
functions are converters from these /simulation types/ to types on the 'Signal' level.
The 'simulateCircuit' function can thus receive the necessary simulation types, convert
them to types on the 'Signal' level, pass those signals to the circuit, and convert the
result of the circuit back to the simulation types giving the final result.
-}
class (C.KnownNat (SimulateChannels a), Protocol a) => Simulate a where
  -- | The type that a test must provide to the 'simulateCircuit' function in the forward direction.
  -- Usually this is some sort of list.
  type SimulateFwdType a :: Type

  -- | The type that a test must provide to the 'simulateCircuit' function in the backward direction.
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

{- | Idle state of a Circuit. Aims to provide no data for both the forward and
backward direction. Transactions are not acknowledged.
-}
class (Protocol p) => IdleCircuit p where
  idleFwd :: Proxy p -> Fwd (p :: Type)
  idleBwd :: Proxy p -> Bwd (p :: Type)

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
    unbundle $
      mux
        rstAsserted
        (bundle (idleBwd $ Proxy @a, idleFwd $ Proxy @a))
        (bundle (bwd, fwd))

#if MIN_VERSION_clash_prelude(1,8,0)
  rstAsserted = unsafeToActiveHigh hasReset
#else
  rstAsserted = unsafeToHighPolarity hasReset
#endif
