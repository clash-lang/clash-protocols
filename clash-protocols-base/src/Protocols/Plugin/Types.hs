{-# LANGUAGE RoleAnnotations #-}
{-# OPTIONS_HADDOCK hide #-}

{- |
These class definitions are needed to be able to write Template Haskell quotes
for instances. They are defined separately to avoid import loops.

This module is not exported; the classes and their (orphan) instances are
exported elsewhere.
-}
module Protocols.Plugin.Types where

import Protocols.Plugin.Cpp
import Clash.Signal
import Data.Kind (Type)
import Protocols.Plugin.TH

import qualified Clash.Prelude as C

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
/some data/. We can capture this in a /Maybe/.

On the way back, we'd like to either acknowledge or not acknowledge sent
data. Similar to /Bool/ we define:

@
  newtype Ack = Ack Bool
@

With these three definitions we're ready to make an instance for /Fwd/ and
/Bwd/:

@
instance Protocol (Df dom a) where
  type Fwd (Df dom a) = Signal dom (Maybe a)
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
      Signal dom (Maybe a)  |           |  Signal dom (Maybe b)
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

{- | Circuit protocol with /Signal dom a/ in its forward direction, and
/()/ in its backward direction. Convenient for exposing protocol
internals, or simply for undirectional streams.
Note: 'CSignal' exists to work around [issue 760](https://github.com/clash-lang/clash-compiler/issues/760)
      in Clash, where type families with 'Signal' on the LHS are broken.
-}
data CSignal (dom :: Domain) (a :: Type)

type role CSignal nominal representational

instance Protocol () where
  type Fwd () = ()
  type Bwd () = ()

{- | __NB__: The documentation only shows instances up to /3/-tuples. By
default, instances up to and including /12/-tuples will exist. If the flag
@large-tuples@ is set instances up to the GHC imposed limit will exist. The
GHC imposed limit is either 62 or 64 depending on the GHC version.
-}
instance Protocol (a, b) where
  type Fwd (a, b) = (Fwd a, Fwd b)
  type Bwd (a, b) = (Bwd a, Bwd b)

-- Generate n-tuple instances, where n > 2
protocolTupleInstances 3 maxTupleSize

instance (C.KnownNat n) => Protocol (C.Vec n a) where
  type Fwd (C.Vec n a) = C.Vec n (Fwd a)
  type Bwd (C.Vec n a) = C.Vec n (Bwd a)

-- XXX: Type families with Signals on LHS are currently broken on Clash:
instance Protocol (CSignal dom a) where
  type Fwd (CSignal dom a) = C.Signal dom a
  type Bwd (CSignal dom a) = C.Signal dom ()
