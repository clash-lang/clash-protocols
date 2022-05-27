{-|
Internal module to prevent hs-boot files (breaks Haddock)
-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE UndecidableInstances #-}

module Protocols.Internal where

import           Data.Proxy
import           GHC.Base (Any)
import           Prelude hiding (map, const)

import           Clash.Prelude (Signal, type (+), type (*))
import qualified Clash.Prelude as C
import qualified Clash.Explicit.Prelude as CE

import           Control.Applicative (Const(..))
import           Data.Coerce (coerce)
import           Data.Default (Default(def))
import           Data.Kind (Type)
import           Data.Tuple (swap)
import           GHC.Generics (Generic)

{- $setup
>>> import Protocols
-}

-- | A /Circuit/, in its most general form, corresponds to a component with two
-- pairs of an input and output. As a diagram:
--
-- @
--             Circuit a b
--
--            +-----------+
--     Fwd a  |           |  Fwd b
--   +------->+           +-------->
--            |           |
--            |           |
--     Bwd a  |           |  Bwd b
--   <--------+           +<-------+
--            |           |
--            +-----------+
-- @
--
-- The first pair, @(Fwd a, Bwd a)@ can be thought of the data sent to and from
-- the component on the left hand side of this circuit. For this pair, @Fwd a@
-- is the data sent from the circuit on the left hand side (not pictured), while
-- @Bwd a@ is the data sent to the left hand side from the current circuit.
--
-- Similarly, the second pair, @(Fwd b, Bwd)@, can be thought of as the data
-- sent to and from the right hand side of this circuit. In this case, @Fwd b@
-- is the data sent from the current circuit to the one on the right hand side,
-- while @Bwd b@ is the data received from the right hand side.
--
-- In Haskell terms, we would say this is simply a function taking two inputs,
-- @Fwd a@ and @Bwd b@, yielding a pair of outputs @Fwd b@ and @Bwd a@. This is
-- in fact exactly its definition:
--
-- @
--   newtype Circuit a b =
--     Circuit ( (Fwd a, Bwd b) -> (Bwd a, Fwd b) )
-- @
--
-- Note that the type parameters /a/ and /b/ don't directly correspond to the
-- types of the inputs and outputs of this function. Instead, the type families
-- @Fwd@ and @Bwd@ decide this. The type parameters can be thought of as
-- deciders for what /protocol/ the left hand side and right hand side must
-- speak.
--
-- Let's make it a bit more concrete by building such a protocol. For this
-- example, we'd like to build a protocol that sends data to a circuit, while
-- allowing the circuit to signal whether it processed the sent data or not. Similarly,
-- we'd like the sender to be able to indicate that it doesn't have any data to
-- send. These kind of protocols fall under the umbrella of "dataflow" protocols,
-- so lets call it /DataFlowSimple/ or /Df/ for short:
--
-- @
--   data Df (dom :: Domain) (a :: Type)
-- @
--
-- We're only going to use it on the type level, so we won't need any
-- constructors for this datatype. The first type parameter indicates the
-- synthesis domain the protocol will use. This is the same /dom/ as used in
-- /Signal dom a/. The second type indicates what data the protocol needs to
-- send. Again, this is similar to the /a/ in /Signal dom a/.
--
-- As said previously, we'd like the sender to either send /no data/ or
-- /some data/. We can capture this in a data type very similar to /Maybe/:
--
-- @
--   data Data a = NoData | Data a
-- @
--
-- On the way back, we'd like to either acknowledge or not acknowledge sent
-- data. Similar to /Bool/ we define:
--
-- @
--   newtype Ack = Ack Bool
-- @
--
-- With these three definitions we're ready to make an instance for /Fwd/ and
-- /Bwd/:
--
-- @
-- instance Protocol (Df dom a) where
--   type Fwd (Df dom a) = Signal dom (Data a)
--   type Bwd (Df dom a) = Signal dom Ack
-- @
--
-- Having defined all this, we can take a look at /Circuit/ once more: now
-- instantiated with our types. The following:
--
-- @
--   f :: Circuit (Df dom a) (Df dom b)
-- @
--
-- ..now corresponds to the following protocol:
--
-- @
--                            +-----------+
--       Signal dom (Data a)  |           |  Signal dom (Data b)
--  +------------------------>+           +------------------------->
--                            |           |
--                            |           |
--       Signal dom Ack       |           |  Signal dom Ack
--  <-------------------------+           +<------------------------+
--                            |           |
--                            +-----------+
-- @
--
-- There's a number of advantages over manually writing out these function
-- types:
--
--   1. It reduces syntactical noise in type signatures
--
--   2. It eliminates the need for manually routing acknowledgement lines
--
newtype Circuit a b =
  Circuit ( (Fwd a, Bwd b) -> (Bwd a, Fwd b) )

-- | Protocol-agnostic acknowledgement
newtype Ack = Ack Bool
  deriving (Generic, C.NFDataX, Show)

-- | Acknowledge. Used in circuit-notation plugin to drive ignore components.
instance Default Ack where
  def = Ack True

-- | Circuit protocol with /CSignal dom a/ in its forward direction, and
-- /CSignal dom ()/ in its backward direction. Convenient for exposing
-- protocol internals.
data CSignal dom a = CSignal (Signal dom a)

instance Default a => Default (CSignal dom a) where
  def = CSignal def

-- | A protocol describes the in- and outputs of one side of a 'Circuit'.
class Protocol a where
  -- | Sender to receiver type family. See 'Circuit' for an explanation on the
  -- existence of 'Fwd'.
  type Fwd (a :: Type)

  -- | Receiver to sender type family. See 'Circuit' for an explanation on the
  -- existence of 'Bwd'.
  type Bwd (a :: Type)

instance Protocol () where
  type Fwd () = ()
  type Bwd () = ()

instance Protocol (a, b) where
  type Fwd (a, b) = (Fwd a, Fwd b)
  type Bwd (a, b) = (Bwd a, Bwd b)

instance Protocol (a, b, c) where
  type Fwd (a, b, c) = (Fwd a, Fwd b, Fwd c)
  type Bwd (a, b, c) = (Bwd a, Bwd b, Bwd c)

instance Protocol (a, b, c, d) where
  type Fwd (a, b, c, d) = (Fwd a, Fwd b, Fwd c, Fwd d)
  type Bwd (a, b, c, d) = (Bwd a, Bwd b, Bwd c, Bwd d)

instance C.KnownNat n => Protocol (C.Vec n a) where
  type Fwd (C.Vec n a) = C.Vec n (Fwd a)
  type Bwd (C.Vec n a) = C.Vec n (Bwd a)

-- XXX: Type families with Signals on LHS are currently broken on Clash:
instance Protocol (CSignal dom a) where
  type Fwd (CSignal dom a) = CSignal dom a
  type Bwd (CSignal dom a) = CSignal dom ()

-- | Left-to-right circuit composition.
--
-- @
--                        Circuit a c
--
--            +---------------------------------+
--
--             Circuit a b    |>    Circuit b c
--
--            +-----------+         +-----------+
--     Fwd a  |           |  Fwd b  |           |  Fwd c
--   +------->+           +-------->+           +-------->
--            |           |         |           |
--            |           |         |           |
--     Bwd a  |           |  Bwd b  |           |  Bwd c
--   <--------+           +<--------+           +<-------+
--            |           |         |           |
--            +-----------+         +-----------+
-- @
--
infixr 1 |>
(|>) :: Circuit a b -> Circuit b c -> Circuit a c
(Circuit fab) |> (Circuit fbc) = Circuit $ \(s2rAc, r2sAc) ->
  let
    ~(r2sAb, s2rAb) = fab (s2rAc, r2sBc)
    ~(r2sBc, s2rBc) = fbc (s2rAb, r2sAc)
  in
    (r2sAb, s2rBc)

-- | Conversion from booleans to protocol specific acknowledgement values.
class Protocol a => Backpressure a where
  -- | Interpret list of booleans as a list of acknowledgements at every cycle.
  -- Implementations don't have to account for finite lists.
  boolsToBwd :: Proxy a -> [Bool] -> Bwd a

instance Backpressure () where
  boolsToBwd _ _ = ()

instance (Backpressure a, Backpressure b) => Backpressure (a, b) where
  boolsToBwd _ bs = (boolsToBwd (Proxy @a) bs, boolsToBwd (Proxy @b) bs)

instance (Backpressure a, Backpressure b, Backpressure c) => Backpressure (a, b, c) where
  boolsToBwd _ bs =
    ( boolsToBwd (Proxy @a) bs
    , boolsToBwd (Proxy @b) bs
    , boolsToBwd (Proxy @c) bs )

instance (C.KnownNat n, Backpressure a) => Backpressure (C.Vec n a) where
  boolsToBwd _ bs = C.repeat (boolsToBwd (Proxy @a) bs)

instance Backpressure (CSignal dom a) where
  boolsToBwd _ _ = CSignal (pure ())

-- | Right-to-left circuit composition.
--
-- @
--                        Circuit a c
--
--            +---------------------------------+
--
--             Circuit b c    <|    Circuit a b
--
--            +-----------+         +-----------+
--     Fwd c  |           |  Fwd b  |           |  Fwd a
--   <--------+           +<--------+           +<-------+
--            |           |         |           |
--            |           |         |           |
--     Bwd c  |           |  Bwd b  |           |  Bwd a
--   +------->+           +-------->+           +-------->
--            |           |         |           |
--            +-----------+         +-----------+
-- @
--
infixr 1 <|
(<|) :: Circuit b c -> Circuit a b -> Circuit a c
(<|) = flip (|>)

-- | View Circuit as its internal representation.
toSignals :: Circuit a b -> ((Fwd a, Bwd b) -> (Bwd a, Fwd b))
toSignals = coerce

-- | View signals as a Circuit
fromSignals :: ((Fwd a, Bwd b) -> (Bwd a, Fwd b)) -> Circuit a b
fromSignals = coerce

-- | Circuit equivalent of 'id'. Useful for explicitly assigning a type to
-- another protocol, or to return a result when using the circuit-notation
-- plugin.
--
-- Examples:
--
-- @
-- idC \@(Df dom a) <| somePolymorphicProtocol
-- @
--
-- @
-- swap :: Circuit (Df dom a, Df dom b) (Df dom b, Df dom a)
-- swap = circuit $ \(a, b) -> do
--   idC -< (b, a)
-- @
--
idC :: forall a. Circuit a a
idC = Circuit swap

-- | Copy a circuit /n/ times. Note that this will copy hardware. If you are
-- looking for a circuit that turns a single channel into multiple, check out
-- 'Protocols.Df.fanout'.
repeatC ::
  forall n a b.
  Circuit a b ->
  Circuit (C.Vec n a) (C.Vec n b)
repeatC (Circuit f) =
  Circuit (C.unzip . C.map f . uncurry C.zip)

-- | Combine two separate circuits into one. If you are looking to combine
-- multiple streams into a single stream, checkout 'Protocols.Df.fanin'.
prod2C ::
  forall a c b d.
  Circuit a b ->
  Circuit c d ->
  Circuit (a, c) (b, d)
prod2C (Circuit a) (Circuit c) =
  Circuit (\((aFwd, cFwd), (bBwd, dBwd)) ->
    let
      (aBwd, bFwd) = a (aFwd, bBwd)
      (cBwd, dFwd) = c (cFwd, dBwd)
    in
      ((aBwd, cBwd), (bFwd, dFwd)))

--------------------------------- SIMULATION -----------------------------------
-- | Specifies option for simulation functions. Don't use this constructor
-- directly, as it may be extend with other options in the future. Use 'def'
-- instead.
data SimulationConfig = SimulationConfig
  { -- | Assert reset for a number of cycles before driving the protocol
    --
    -- Default: 100
    resetCycles :: Int

    -- | Timeout after /n/ cycles. Only affects sample functions.
    --
    -- Default: 'maxBound'
  , timeoutAfter :: Int

    -- | Ignore cycles while in reset (sampleC)
    --
    -- Default: False
  , ignoreReset :: Bool
  }
  deriving (Show)

instance Default SimulationConfig where
  def = SimulationConfig
    { resetCycles = 100
    , timeoutAfter = maxBound
    , ignoreReset = False }

-- | Determines what kind of acknowledgement signal 'stallC' will send when its
-- input component is not sending any data. Note that, in the Df protocol,
-- protocols may send arbitrary acknowledgement signals when this happens.
data StallAck
  -- | Send Nack
  = StallWithNack
  -- | Send Ack
  | StallWithAck
  -- | Send @errorX "No defined ack"@
  | StallWithErrorX
  -- | Passthrough acknowledgement of RHS component
  | StallTransparently
  -- | Cycle through all modes
  | StallCycle
  deriving (Eq, Bounded, Enum, Show)

-- | Class that defines how to /drive/, /sample/, and /stall/ a "Circuit" of
-- some shape. The "Backpressure" instance requires that the /backward/ type of the
-- circuit can be generated from a list of Booleans.
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


-- | Defines functions necessary for implementation of the 'simulateCircuit' function. This
-- kind of simulation requires a lists for both the forward and the backward direction.
--
-- This class requires the definition of the types that the test supplies and returns. Its
-- functions are converters from these /simulation types/ to types on the 'Signal' level.
-- The 'simulateCircuit' function can thus receive the necessary simulation types, convert
-- them to types on the 'Signal' level, pass those signals to the circuit, and convert the
-- result of the circuit back to the simulation types giving the final result.
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
  --
  stallC ::
    SimulationConfig ->
    C.Vec (SimulateChannels a) (StallAck, [Int]) ->
    Circuit a a


instance Simulate () where
  type SimulateFwdType () = ()
  type SimulateBwdType () = ()
  type SimulateChannels () = 0

  simToSigFwd _ = id
  simToSigBwd _ = id
  sigToSimFwd _ = id
  sigToSimBwd _ = id

  stallC _ _ = idC

instance Drivable () where
  type ExpectType () = ()

  toSimulateType Proxy () = ()
  fromSimulateType Proxy () = ()

  driveC _ _ = idC
  sampleC _  _ = ()


instance (Simulate a, Simulate b) => Simulate (a, b) where
  type SimulateFwdType (a, b) = (SimulateFwdType a, SimulateFwdType b)
  type SimulateBwdType (a, b) = (SimulateBwdType a, SimulateBwdType b)
  type SimulateChannels (a, b) = SimulateChannels a + SimulateChannels b

  simToSigFwd Proxy (fwdsA, fwdsB) = (simToSigFwd (Proxy @a) fwdsA, simToSigFwd (Proxy @b) fwdsB)
  simToSigBwd Proxy (bwdsA, bwdsB) = (simToSigBwd (Proxy @a) bwdsA, simToSigBwd (Proxy @b) bwdsB)
  sigToSimFwd Proxy (fwdSigA, fwdSigB) = (sigToSimFwd (Proxy @a) fwdSigA, sigToSimFwd (Proxy @b) fwdSigB)
  sigToSimBwd Proxy (bwdSigA, bwdSigB) = (sigToSimBwd (Proxy @a) bwdSigA, sigToSimBwd (Proxy @b) bwdSigB)

  stallC conf stalls =
    let
      (stallsL, stallsR) = C.splitAtI @(SimulateChannels a) @(SimulateChannels b) stalls
      Circuit stalledL = stallC @a conf stallsL
      Circuit stalledR = stallC @b conf stallsR
    in
      Circuit $ \((fwdL0, fwdR0), (bwdL0, bwdR0)) ->
        let
          (fwdL1, bwdL1) = stalledL (fwdL0, bwdL0)
          (fwdR1, bwdR1) = stalledR (fwdR0, bwdR0)
        in
          ((fwdL1, fwdR1), (bwdL1, bwdR1))

instance (Drivable a, Drivable b) => Drivable (a, b) where
  type ExpectType (a, b) = (ExpectType a, ExpectType b)

  toSimulateType Proxy (t1, t2) =
    ( toSimulateType (Proxy @a) t1
    , toSimulateType (Proxy @b) t2 )

  fromSimulateType Proxy (t1, t2) =
    ( fromSimulateType (Proxy @a) t1
    , fromSimulateType (Proxy @b) t2 )

  driveC conf (fwd1, fwd2) =
    let (Circuit f1, Circuit f2) = (driveC @a conf fwd1, driveC @b conf fwd2) in
    Circuit (\(_, ~(bwd1, bwd2)) -> ((), (snd (f1 ((), bwd1)), snd (f2 ((), bwd2)))))

  sampleC conf (Circuit f) =
    let
      bools = replicate (resetCycles conf) False <> repeat True
      (_, (fwd1, fwd2)) = f ((), (boolsToBwd (Proxy @a) bools, boolsToBwd (Proxy @b) bools))
    in
      ( sampleC @a conf (Circuit $ \_ -> ((), fwd1))
      , sampleC @b conf (Circuit $ \_ -> ((), fwd2)) )


-- TODO TemplateHaskell?
-- instance SimulateType (a, b, c)
-- instance SimulateType (a, b, c, d)

instance (CE.KnownNat n, Simulate a) => Simulate (C.Vec n a) where
  type SimulateFwdType (C.Vec n a) = C.Vec n (SimulateFwdType a)
  type SimulateBwdType (C.Vec n a) = C.Vec n (SimulateBwdType a)
  type SimulateChannels (C.Vec n a) = n * SimulateChannels a

  simToSigFwd Proxy = C.map (simToSigFwd (Proxy @a))
  simToSigBwd Proxy = C.map (simToSigBwd (Proxy @a))
  sigToSimFwd Proxy = C.map (sigToSimFwd (Proxy @a))
  sigToSimBwd Proxy = C.map (sigToSimBwd (Proxy @a))

  stallC conf stalls0 =
    let
      stalls1 = C.unconcatI @n @(SimulateChannels a) stalls0
      stalled = C.map (toSignals . stallC @a conf) stalls1
    in
      Circuit $ \(fwds, bwds) -> C.unzip (C.zipWith ($) stalled (C.zip fwds bwds))

instance (C.KnownNat n, Drivable a) => Drivable (C.Vec n a) where
  type ExpectType (C.Vec n a) = C.Vec n (ExpectType a)

  toSimulateType Proxy = C.map (toSimulateType (Proxy @a))
  fromSimulateType Proxy = C.map (fromSimulateType (Proxy @a))

  driveC conf fwds =
    let circuits = C.map (($ ()) . curry . (toSignals @_ @a) . driveC conf) fwds in
    Circuit (\(_, bwds) -> ((), C.map snd (C.zipWith ($) circuits bwds)))

  sampleC conf (Circuit f) =
    let
      bools = replicate (resetCycles conf) False <> repeat True
      (_, fwds) = f ((), (C.repeat (boolsToBwd (Proxy @a) bools)))
    in
      C.map (\fwd -> sampleC @a conf (Circuit $ \_ -> ((), fwd))) fwds


instance (C.KnownDomain dom) => Simulate (CSignal dom a) where
  type SimulateFwdType (CSignal dom a) = [a]
  type SimulateBwdType (CSignal dom a) = ()
  type SimulateChannels (CSignal dom a) = 1

  simToSigFwd Proxy list = CSignal (C.fromList_lazy list)
  simToSigBwd Proxy () = def
  sigToSimFwd Proxy (CSignal sig) = C.sample_lazy sig
  sigToSimBwd Proxy _ = ()

  stallC _ _ = idC

instance Default (CSignal dom (Const () a)) where
  def = CSignal (pure (Const ()))

instance (C.NFDataX a, C.ShowX a, Show a, C.KnownDomain dom) => Drivable (CSignal dom a) where
  type ExpectType (CSignal dom a) = [a]

  toSimulateType Proxy = id
  fromSimulateType Proxy = id

  driveC _conf [] = error "CSignal.driveC: Can't drive with empty list"
  driveC SimulationConfig{resetCycles} fwd0@(f:_) =
    let fwd1 = C.fromList_lazy (replicate resetCycles f <> fwd0 <> repeat f) in
    Circuit ( \_ -> ((), CSignal fwd1) )

  sampleC SimulationConfig{resetCycles, ignoreReset} (Circuit f) =
    let sampled = CE.sample_lazy ((\(CSignal s) -> s) (snd (f ((), def)))) in
    if ignoreReset then drop resetCycles sampled else sampled


-- | Simulate a circuit. Includes samples while reset is asserted.
-- Not synthesizable.
--
-- To figure out what input you need to supply, either solve the type
-- "SimulateFwdType" manually, or let the repl do the work for you! Example:
--
-- >>> :kind! (forall dom a. SimulateFwdType (Df dom a))
-- ...
-- = [Protocols.Df.Data a]
--
-- This would mean a @Circuit (Df dom a) (Df dom b)@ would need
-- @[Data a]@ as the last argument of 'simulateC' and would result in
-- @[Data b]@. Note that for this particular type you can neither supply
-- stalls nor introduce backpressure. If you want to to this use 'Df.stall'.
simulateC ::
  forall a b.
  (Drivable a, Drivable b) =>
  -- | Circuit to simulate
  Circuit a b ->
  -- | Simulation configuration. Note that some options only apply to 'sampleC'
  -- and some only to 'driveC'.
  SimulationConfig ->
  -- | Circuit input
  SimulateFwdType a ->
  -- | Circuit output
  SimulateFwdType b
simulateC c conf as =
  sampleC conf (driveC conf as |> c)

-- | Like 'simulateC', but does not allow caller to control and observe
-- backpressure. Furthermore, it ignores all data produced while the reset is
-- asserted.
--
-- Example:
--
-- >>> import qualified Protocols.Df as Df
-- >>> take 2 (simulateCS (Df.catMaybes @C.System @Int) [Nothing, Just 1, Nothing, Just 3])
-- [1,3]
simulateCS ::
  forall a b.
  (Drivable a, Drivable b) =>
  -- | Circuit to simulate
  Circuit a b ->
  -- | Circuit input
  ExpectType a ->
  -- | Circuit output
  ExpectType b
simulateCS c =
     fromSimulateType (Proxy @b)
   . simulateC c def{ignoreReset=True}
   . toSimulateType (Proxy @a)

-- | Like 'simulateCS', but takes a circuit expecting a clock, reset, and enable.
simulateCSE ::
  forall dom a b.
  (Drivable a, Drivable b, C.KnownDomain dom) =>
  -- | Circuit to simulate
  (C.Clock dom -> C.Reset dom -> C.Enable dom -> Circuit a b) ->
  -- | Circuit input
  ExpectType a ->
  -- | Circuit output
  ExpectType b
simulateCSE c = simulateCS (c clk rst ena)
 where
  clk = C.clockGen
  rst = resetGen (resetCycles def)
  ena = C.enableGen

  resetGen n = C.unsafeFromHighPolarity (C.fromList (replicate n True <> repeat False))

-- | Applies conversion functions defined in the 'Simulate' instance of @a@ and @b@ to
-- the given simulation types, and applies the results to the internal function of the
-- given 'Circuit'. The resulting internal types are converted to the simulation types.
simulateCircuit :: forall a b . (Simulate a, Simulate b) =>
  SimulateFwdType a -> SimulateBwdType b ->
  Circuit a b ->
  (SimulateBwdType a, SimulateFwdType b)
simulateCircuit fwds bwds circ =
    (sigToSimBwd (Proxy @a) bwdSig, sigToSimFwd (Proxy @b) fwdSig)
    where
      (bwdSig, fwdSig) = toSignals circ $
        (simToSigFwd (Proxy @a) fwds, simToSigBwd (Proxy @b) bwds)


-- | Picked up by "Protocols.Plugin" to process protocol DSL. See
-- "Protocols.Plugin" for more information.
circuit :: Any
circuit =
  error "'protocol' called: did you forget to enable \"Protocols.Plugin\"?"

-- | Picked up by "Protocols.Plugin" to tie circuits together. See
-- "Protocols.Plugin" for more information.
(-<) :: Any
(-<) =
  error "(-<) called: did you forget to enable \"Protocols.Plugin\"?"

data Reverse a

instance Protocol a => Protocol (Reverse a) where
  type Fwd (Reverse a) = Bwd a
  type Bwd (Reverse a) = Fwd a
