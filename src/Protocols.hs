{-|
See 'Circuit' for documentation. This module is designed to import unqualified,
i.e. using:

@
  import Protocols
@

Definitions of 'Circuit', 'Fwd', 'Bwd', 'Protocols.Dfs.Dfs', inspired by
definitions in @circuit-notation@ at <https://github.com/cchalmers/circuit-notation>.
-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE UndecidableInstances #-}

module Protocols
  ( -- * Circuit definition
    Circuit(Circuit)
  , Protocol(Fwd, Bwd)
  , Ack(..)

    -- * Combinators & functions
  , (|>), (<|)
  , fromSignals, toSignals

    -- * Basic protocols
  , idC
  , repeatC

    -- * Simulation
  , Simulate(SimulateType, SimulateChannels, driveC, sampleC, stallC)
  , SimulationConfig(..)
  , StallAck(..)
  , simulateC

    -- * Circuit notation plugin
  , circuit
  ) where

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

-- | A /protocol/, in its most general form, corresponds to a component with two
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
-- the component on the left hand side of this protocol. For this pair, @Fwd a@
-- is the data sent from the protocol on the left hand side (not pictured), while
-- @Bwd a@ is the data sent to the left hand side from the current protocol.
--
-- Similarly, the second pair, @(Fwd b, Bwd)@, can be thought of as the data
-- sent to and from the right hand side of this protocol. In this case, @Fwd b@
-- is the data sent from the current protocol to the one on the right hand side,
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
-- example, we'd like to build a protocol that sends data to a protocol, while
-- allowing the protocol to signal whether it processed the sent data or not. Similarly,
-- we'd like the sender to be able to indicate that it doesn't have any data to
-- send. These kind of protocols fall under the umbrella of "dataflow" protocols,
-- so lets call it /DataFlowSimple/ or /Dfs/ for short:
--
-- @
--   data Dfs (dom :: Domain) (a :: Type)
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
--   data Ack a = DfNack | Ack
-- @
--
-- (For technical reasons[1] we need the type variable /a/ in this definition,
-- even though we don't use it on the right hand side.)
--
-- With these three definitions we're ready to make an instance for /Fwd/ and
-- /Bwd/:
--
-- @
--   instance Fwd (Dfs dom a) = Signal dom (Data a)
--   instance Bwd (Dfs dom a) = Signal dom (Ack a)
-- @
--
-- Having defined all this, we can take a look at /Circuit/ once more: now
-- instantiated with our types. The following:
--
-- @
--   f :: Circuit (Dfs dom a) (Dfs dom b)
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
--       Signal dom (Ack a)   |           |  Signal dom (Ack b)
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
-- Footnotes:
--
-- 1. Fwd and Bwd are injective type families. I.e., something on
--    the right hand side of a type instance must uniquely identify the left
--    hand side and vice versa. This helps type inference and error messages
--    substantially, in exchange for a slight syntactical artifact. As a
--    result, any type variables on the left hand side must occur on the right
--    hand side too.
--
newtype Circuit a b =
  Circuit ( (Fwd a, Bwd b) -> (Bwd a, Fwd b) )

-- | Protocol-agnostic acknowledgement
newtype Ack = Ack Bool
  deriving (Generic, C.NFDataX)

-- | Circuit protocol with /CSignal dom a/ in its forward direction, and
-- /CSignal dom ()/ in its backward direction. Convenient for exposing
-- protocol internals.
data CSignal dom a = CSignal (Signal dom a)

-- | A protocol describes the in- and outputs of one side of a 'Circuit'.
class Protocol a where
  -- | Sender to receiver type family. See 'Circuit' for an explanation on the
  -- existence of 'Fwd'.
  type Fwd (a :: Type) = (r :: Type) | r -> a

  -- | Receiver to sender type family. See 'Circuit' for an explanation on the
  -- existence of 'Bwd'.
  type Bwd (a :: Type) = (r :: Type) | r -> a

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

instance Protocol (C.Vec n a) where
  type Fwd (C.Vec n a) = C.Vec n (Fwd a)
  type Bwd (C.Vec n a) = C.Vec n (Bwd a)

-- XXX: Type families with Signals on LHS are currently broken on Clash:
instance Protocol (CSignal dom a) where
  type Fwd (CSignal dom a) = CSignal dom a
  type Bwd (CSignal dom a) = CSignal dom (Const () a)

-- | Circuit combinator
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

-- | Flipped protocol combinator
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
-- swap :: Circuit (Dfs dom a, Dfs dom b) (Dfs dom b, Dfs dom a)
-- swap = circuit $ \(a, b) -> do
--   idC -< (b, a)
-- @
--
idC :: forall a. Circuit a a
idC = Circuit swap

-- | Copy a protocol /n/ times. Note that this will copy hardware. If you are
-- looking for a circuit that turns a single channel into multiple, check out
-- 'Protocols.Df.fanout' or 'Protocols.Df.Simple.fanout'.
repeatC ::
  forall n a b.
  Circuit a b ->
  Circuit (C.Vec n a) (C.Vec n b)
repeatC (Circuit f) =
  Circuit (C.unzip . C.map f . uncurry C.zip)

--------------------------------- SIMULATION -----------------------------------
-- | Specifies option for simulation functions. Don't use this constructor
-- directly, as it may be extend with other options in the future. Use 'def'
-- instead.
data SimulationConfig = SimulationConfig
  { -- | Assert reset for a number of cycles before driving the protocol
    resetCycles :: Int

    -- | Timeout after /n/ cycles. Only affects sample functions.
  , timeoutAfter :: Int
  }
  deriving (Show)

instance Default SimulationConfig where
  def = SimulationConfig
    { resetCycles = 100
    , timeoutAfter = maxBound }

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
 -- some shape.
class (C.KnownNat (SimulateChannels a), Default (Bwd a)) => Simulate a where
  -- Type a /Circuit/ driver needs or sampler yields. For example:
  --
  -- >>> :kind! (forall dom a. SimulateType (Dfs dom a))
  -- ...
  -- = [Maybe (meta, a)]
  --
  -- This means sampling a @Circuit () (Dfs dom a)@ yields @[Maybe a]@.
  type SimulateType a :: Type

  -- | The number of simulation channel this channel has after flattening it.
  -- For example, @(Dfs dom a, Dfs dom a)@ has 2, while
  -- @Vec 4 (Dfs dom a, Dfs dom a)@ has 8.
  type SimulateChannels a :: C.Nat

  -- | Create a /driving/ protocol component. Can be used in combination with
  -- 'sampleC' to simulate a protocol component. Related: 'simulateC'.
  driveC ::
    SimulationConfig ->
    SimulateType a ->
    Circuit () a

  -- | Sample a protocol component that is trivially drivable. Use 'driveC'
  -- to create such a component. Related: 'simulateC'.
  sampleC ::
    SimulationConfig ->
    Circuit () a ->
    SimulateType a

  -- | Create a /stalling/ protocol component. For each simulation channel
  -- (see 'SimulateChannels') a tuple determines how the component stalls:
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
  type SimulateType () = ()
  type SimulateChannels () = 0

  driveC _ _ = idC
  sampleC _  _ = ()
  stallC _ _ = idC

instance (Simulate a, Simulate b) => Simulate (a, b) where
  type SimulateType (a, b) = (SimulateType a, SimulateType b)
  type SimulateChannels (a, b) = SimulateChannels a + SimulateChannels b

  driveC conf (fwd1, fwd2) =
    let (Circuit f1, Circuit f2) = (driveC conf fwd1, driveC conf fwd2) in
    Circuit (\(_, (bwd1, bwd2)) -> ((), (snd (f1 ((), bwd1)), snd (f2 ((), bwd2)))))

  sampleC conf (Circuit f) =
    let (_, (fwd1, fwd2)) = f ((), (def, def)) in
    ( sampleC conf (Circuit $ \_ -> ((), fwd1))
    , sampleC conf (Circuit $ \_ -> ((), fwd2)) )

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

-- TODO TemplateHaskell?
-- instance SimulateType (a, b, c)
-- instance SimulateType (a, b, c, d)

instance (C.KnownNat n, Simulate a) => Simulate (C.Vec n a) where
  type SimulateType (C.Vec n a) = C.Vec n (SimulateType a)
  type SimulateChannels (C.Vec n a) = n * SimulateChannels a

  driveC conf fwds =
    let protocols = C.map (($ ()) . curry . toSignals . driveC conf) fwds in
    Circuit (\(_, bwds) -> ((), C.map snd (C.zipWith ($) protocols bwds)))

  sampleC conf (Circuit f) =
    let (_, fwds) = f ((), (C.repeat def)) in
    C.map (\fwd -> sampleC conf (Circuit $ \_ -> ((), fwd))) fwds

  stallC conf stalls0 =
    let
      stalls1 = C.unconcatI @n @(SimulateChannels a) stalls0
      stalled = C.map (toSignals . stallC @a conf) stalls1
    in
      Circuit $ \(fwds, bwds) -> C.unzip (C.zipWith ($) stalled (C.zip fwds bwds))

instance Default (CSignal dom (Const () a)) where
  def = CSignal (pure (Const ()))

instance (C.NFDataX a, C.ShowX a, Show a) => Simulate (CSignal dom a) where
  type SimulateType (CSignal dom a) = [a]
  type SimulateChannels (CSignal dom a) = 1

  driveC _conf [] = error "CSignal.driveC: Can't drive with empty list"
  driveC SimulationConfig{resetCycles} fwd0@(f:_) =
    let fwd1 = C.fromList_lazy (replicate resetCycles f <> fwd0 <> repeat f) in
    Circuit ( \_ -> ((), CSignal fwd1) )

  sampleC SimulationConfig{resetCycles} (Circuit f) =
    drop resetCycles (CE.sample_lazy ((\(CSignal s) -> s) (snd (f ((), def)))))

  stallC _ _ = idC

-- | Simulate a protocol. Not synthesizable.
--
-- To figure out what input you need to supply, either solve the type
-- "SimulateType" manually, or let the repl do the work for you! Example:
--
-- >>> :kind! (forall dom meta a. SimulateType (Df dom meta a))
-- ...
-- = [Maybe (meta, a)]
--
-- This would mean a @Circuit (Df dom meta a) (Df dom meta b)@ would need
-- @[(meta, a)]@ as the last argument of 'simulateC' and would result in
-- @[(meta, b)]@. Note that for this particular type you can neither supply
-- stalls nor introduce backpressure. If you want to to this use 'Df.stall'.
simulateC ::
  forall a b.
  (Simulate a, Simulate b) =>
  -- | Circuit to simulate
  Circuit a b ->
  -- | Simulation configuration. Note that some options only apply to 'sampleC'
  -- and some only to 'driveC'.
  SimulationConfig ->
  -- | Circuit input
  SimulateType a ->
  -- | Circuit output
  SimulateType b
simulateC c conf as =
  sampleC conf (driveC conf as |> c)

-- | Picked up by "Protocols.Plugin" to process protocol DSL. See
-- "Protocols.Plugin" for more information.
circuit :: Any
circuit =
  error "'protocol' called: did you forget to enable \"Protocols.Plugin\"?"
