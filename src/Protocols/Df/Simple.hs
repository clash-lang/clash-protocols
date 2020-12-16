{-|
Defines data structures and operators to create a Dataflow protocol that only
carries data, no metadata. For documentation see:

  * 'Protocols.Circuit'
  * 'Protocols.Df.Simple.Dfs'

-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module Protocols.Df.Simple where

import Clash.Signal.Internal (Signal(..))

import           Prelude hiding ((!!), map, zip, zipWith, filter)

import           Control.Applicative (Alternative((<|>)), Applicative(liftA2))
import qualified Data.Bifunctor as B
import qualified Data.Bifunctor.Extra as B
import           Data.Bool (bool)
import           Data.Coerce (coerce)
import           Data.Default (Default)
import           Data.Kind (Type)
import qualified Data.Tuple.Extra as T
import qualified Data.List.NonEmpty
import           Data.Maybe (mapMaybe, fromMaybe)
import           Data.Proxy
import qualified Prelude as P

import           Clash.Prelude (type (<=), type (-), type (+), (!!))
import qualified Clash.Prelude as C

import           Protocols.Internal hiding (Ack(..))
import qualified Protocols.Internal as Protocols
import qualified Protocols.Df as Df
import           Protocols.Df (Df)
import qualified Protocols.DfLike as DfLike
import           Protocols.DfLike (DfLike)

import           Control.DeepSeq (NFData)
import           GHC.Generics (Generic)
import           GHC.Stack (HasCallStack)

-- $setup
-- >>> import Protocols
-- >>> import Clash.Prelude (Vec(..))
-- >>> import qualified Prelude as P

-- | Like 'Protocols.Df', but without metadata.
--
-- __N.B.__: For performance reasons 'Protocols.Df.Simple.Data' is strict on
-- its data field. That is, if 'Protocols.Df.Simple.Data' is evaluated to WHNF,
-- its fields will be evaluated to WHNF too.
data Dfs (dom :: C.Domain) (a :: Type)

instance Protocol (Dfs dom a) where
  -- | Forward part of simple dataflow: @Signal dom (Data meta a)@
  type Fwd (Dfs dom a) = Signal dom (Data a)

  -- | Backward part of simple dataflow: @Signal dom (Ack meta a)@
  type Bwd (Dfs dom a) = Signal dom (Ack a)

instance Backpressure (Dfs dom a) where
  boolsToBwd = C.fromList_lazy . coerce

-- | Data sent over forward channel of 'Dfs'. Note that this data type is strict
-- on its data field. If you need lazy behavior, check out
-- "Protocols.Df.Simple.Lazy".
data Data a
  -- | Send no data
  = NoData
  -- | Send /a/
  | Data !a
  deriving (Functor, Generic, C.NFDataX, C.ShowX, Eq, NFData, Show)

instance Applicative Data where
  pure = Data

  liftA2 f (Data a) (Data b) = Data (f a b)
  liftA2 _ _        _        = NoData

instance Alternative Data where
  empty = NoData

  Data a <|> _ = Data a
  _      <|> b = b

instance Monad Data where
  (>>=) :: Data a -> (a -> Data b) -> Data b
  NoData >>= _f = NoData
  Data a >>=  f = f a

-- | Convert 'Data' to 'Maybe'. Produces 'Just' on 'Data', 'Nothing' on 'NoData'.
dataToMaybe :: Data a -> Maybe a
dataToMaybe NoData = Nothing
dataToMaybe (Data a) = Just a

-- | Constructed with 'Data'?
isData :: Data a -> Bool
isData (Data _) = True
isData NoData = False

-- | Constructed with 'NoData'?
isNoData :: Data a -> Bool
isNoData = not . isData

-- | Like 'Protocols.Df.Ack', but carrying phantom type variables to satisfy
-- 'Bwd's injectivity requirement.
newtype Ack a = Ack Bool
  deriving (Show)

instance Default (Ack a) where
  def = Ack True

instance (C.KnownDomain dom, C.NFDataX a, C.ShowX a, Show a) => Simulate (Dfs dom a) where
  type SimulateType (Dfs dom a) = [Data a]
  type ExpectType (Dfs dom a) = [a]
  type SimulateChannels (Dfs dom a) = 1

  toSimulateType Proxy = P.map Data
  fromSimulateType Proxy = mapMaybe dataToMaybe

  driveC = drive
  sampleC = sample
  stallC conf (C.head -> (stallAck, stalls)) = stall conf stallAck stalls

instance DfLike dom (Dfs dom a) where
  type Payload (Dfs dom a)  = a
  type Meta (Dfs dom a) = ()

  fromDf =
    let go = \case {Df.Data () a -> Data a; Df.NoData -> NoData} in
    Df.mapInternal (B.swapMap go coerce)

  toDf =
    let go = \case {Data a -> Df.Data () a; NoData -> Df.NoData} in
    Df.mapInternal (B.swapMap go coerce)

-- | Interpret simple dataflow carrying a tuple as 'Df' with /meta/ and /payload/
asDf :: Circuit (Dfs dom (meta, payload)) (Df dom meta payload)
asDf = Df.mapInternal (B.swapMap go coerce)
 where
  go (Data (meta, a)) = Df.Data meta a
  go NoData = Df.NoData

-- | Interpret 'Df' as simple dataflow carrying a tuple of /meta/ and /payload/
asDfs :: Circuit (Df dom meta payload) (Dfs dom (meta, payload))
asDfs = Df.mapInternal (B.swapMap go coerce)
 where
  go (Df.Data meta a) = Data (meta, a)
  go Df.NoData = NoData

-- | Force a /nack/ on the backward channel and /no data/ on the forward
-- channel if reset is asserted.
forceAckLow :: C.HiddenClockResetEnable dom => Circuit (Dfs dom a) (Dfs dom a)
forceAckLow = DfLike.forceAckLow

-- | Like 'P.map', but over payload (/a/) of a Dfs stream.
map :: (a -> b) -> Circuit (Dfs dom a) (Dfs dom b)
map = DfLike.map

-- | Like 'P.fst', but over payload of a Dfs stream.
fst :: Circuit (Dfs dom (a, b)) (Dfs dom a)
fst = DfLike.fst

-- | Like 'P.snd', but over payload of a Dfs stream.
snd :: Circuit (Dfs dom (a, b)) (Dfs dom b)
snd = DfLike.snd

-- | Like 'Data.Bifunctor.first', but over payload of a Dfs stream.
first :: (a -> b) -> Circuit (Dfs dom (a, c)) (Dfs dom (b, c))
first = DfLike.first

-- | Like 'Data.Bifunctor.second', but over payload of a Dfs stream.
second :: (b -> c) -> Circuit (Dfs dom (a, b)) (Dfs dom (a, c))
second = DfLike.second

-- | Acknowledge but ignore data from LHS protocol. Send a static value /b/.
const :: b -> Circuit (Dfs dom a) (Dfs dom b)
const = DfLike.const ()

-- | Drive a constant value composed of /a/.
pure :: a -> Circuit () (Dfs dom a)
pure = DfLike.pure ()

-- | Like 'Data.Maybe.catMaybes', but over a Dfs stream.
--
-- Example:
--
-- >>> take 2 (simulateCS (catMaybes @C.System @Int) [Nothing, Just 1, Nothing, Just 3])
-- [1,3]
--
catMaybes :: Circuit (Dfs dom (Maybe a)) (Dfs dom a)
catMaybes = DfLike.catMaybes

-- | Like 'P.filter', but over a 'Dfs' stream.
--
-- Example:
--
-- >>> take 3 (simulateCS (filter @C.System @Int (>5)) [1, 5, 7, 10, 3, 11])
-- [7,10,11]
--
filter :: forall dom a. (a -> Bool) -> Circuit (Dfs dom a) (Dfs dom a)
filter = DfLike.filter

-- | Like 'Data.Either.Combinators.mapLeft', but over payload of a 'Dfs' stream.
mapLeft :: (a -> b) -> Circuit (Dfs dom (Either a c)) (Dfs dom (Either b c))
mapLeft = DfLike.mapLeft

-- | Like 'Data.Either.Combinators.mapRight', but over payload of a 'Dfs' stream.
mapRight :: (b -> c) -> Circuit (Dfs dom (Either a b)) (Dfs dom (Either a c))
mapRight = DfLike.mapRight

-- | Like 'Data.Either.either', but over a 'Dfs' stream.
either :: (a -> c) -> (b -> c) -> Circuit (Dfs dom (Either a b)) (Dfs dom c)
either f g = DfLike.map (P.either f g)

-- | Like 'P.zipWith', but over two 'Dfs' streams.
--
-- Example:
--
-- >>> take 3 (simulateCS (zipWith @C.System @Int (+)) ([1, 3, 5], [2, 4, 7]))
-- [3,7,12]
--
zipWith ::
  forall dom a b c.
  (a -> b -> c) ->
  Circuit
    (Dfs dom a, Dfs dom b)
    (Dfs dom c)
zipWith f =
  Circuit (B.first C.unbundle . C.unbundle . fmap go . C.bundle . B.first C.bundle)
 where
  go ((Data a, Data b), Ack ack) = ((Ack ack, Ack ack), Data (f a b))
  go _ = ((Ack False, Ack False), NoData)

-- | Like 'P.zip', but over two 'Dfs' streams.
zip :: forall a b dom. Circuit (Dfs dom a, Dfs dom b) (Dfs dom (a, b))
zip = zipWith (,)

-- | Like 'P.partition', but over 'Dfs' streams
--
-- Example:
--
-- >>> let input = [1, 3, 5, 7, 9, 2, 11]
-- >>> let output = simulateCS (partition @C.System @Int (>5)) input
-- >>> B.bimap (take 3) (take 4) output
-- ([7,9,11],[1,3,5,2])
--
partition :: forall dom a. (a -> Bool) -> Circuit (Dfs dom a) (Dfs dom a, Dfs dom a)
partition f =
  Circuit (B.second C.unbundle . C.unbundle . fmap go . C.bundle . B.second C.bundle)
 where
  go (Data a, (ackT, ackF))
    | f a       = (ackT, (Data a, NoData))
    | otherwise = (ackF, (NoData, Data a))
  go _ = (Ack False, (NoData, NoData))

-- | Route a 'Dfs' stream to another corresponding to the index
--
-- Example:
--
-- >>> let input = [(0, 3), (0, 5), (1, 7), (2, 13), (1, 11), (2, 1)]
-- >>> let output = simulateCS (route @3 @C.System @Int) input
-- >>> fmap (take 2) output
-- <[3,5],[7,11],[13,1]>
--
route ::
  forall n dom a. C.KnownNat n =>
  Circuit (Dfs dom (C.Index n, a)) (C.Vec n (Dfs dom a))
route =
  Circuit (B.second C.unbundle . C.unbundle . fmap go . C.bundle . B.second C.bundle)
 where
  go :: (Data (C.Index n, a), C.Vec n (Ack a)) -> (Ack (C.Index n, a), C.Vec n (Data a))
  go (Data (i, a), acks) = (coerce (acks C.!! i), C.replace i (Data a) (C.repeat NoData))
  go _ = (Ack False, C.repeat NoData)

-- | Select data from the channel indicated by the 'Dfs' stream carrying
-- @Index n@.
--
-- Example:
--
-- >>> let indices = [1, 1, 2, 0, 2]
-- >>> let dats = [8] :> [5, 7] :> [9, 1] :> Nil
-- >>> let output = simulateCS (select @3 @C.System @Int) (dats, indices)
-- >>> take 5 output
-- [5,7,9,8,1]
--
select ::
  forall n dom a.
  C.KnownNat n =>
  Circuit (C.Vec n (Dfs dom a), Dfs dom (C.Index n)) (Dfs dom a)
select = selectUntil (P.const True)

-- | Select /selectN/ samples from channel /n/.
--
-- Example:
--
-- >>> let indices = [(0, 2), (1, 3), (0, 2)]
-- >>> let dats = [10, 20, 30, 40] :> [11, 22, 33] :> Nil
-- >>> let circuit = C.exposeClockResetEnable (selectN @2 @10 @C.System @Int)
-- >>> take 7 (simulateCSE circuit (dats, indices))
-- [10,20,11,22,33,30,40]
--
selectN ::
  forall n selectN dom a.
  ( C.HiddenClockResetEnable dom
  , C.KnownNat selectN
  , C.KnownNat n
  ) =>
  Circuit
    (C.Vec n (Dfs dom a), Dfs dom (C.Index n, C.Index selectN))
    (Dfs dom a)
selectN = Circuit $
    B.first (B.first C.unbundle . C.unbundle)
  . C.mealyB go (0 :: C.Index (selectN + 1))
  . B.first (C.bundle . B.first C.bundle)
 where
  go c0 ((dats, dat), Ack iAck)
    -- Select zero samples: don't send any data to RHS, acknowledge index stream
    -- but no data stream.
    | Data (_, 0) <- dat
    = (c0, ((nacks, Ack True), NoData))

    -- Acknowledge data if RHS acknowledges ours. Acknowledge index stream if
    -- we're done.
    | Data (streamI, nSelect) <- dat
    , Data d <- dats C.!! streamI
    = let
        c1 = if iAck then succ c0 else c0
        oAckIndex = c1 == C.extend nSelect
        c2 = if oAckIndex then 0 else c1
        datAcks = C.replace streamI (Ack iAck) nacks
      in
        (c2, ((datAcks, Ack oAckIndex), Data d))

    -- No index from LHS, nothing to do
    | otherwise
    = (c0, ((nacks, Ack False), NoData))
   where
    nacks = C.repeat (Ack False)

-- | Selects samples from channel /n/ until the predicate holds. The cycle in
-- which the predicate turns true is included.
--
-- Example:
--
-- >>> let indices = [0, 0, 1, 2]
-- >>> let channel1 = [(10, False), (20, False), (30, True), (40, True)]
-- >>> let channel2 = [(11, False), (21, True)]
-- >>> let channel3 = [(12, False), (22, False), (32, False), (42, True)]
-- >>> let dats = channel1 :> channel2 :> channel3 :> Nil
-- >>> take 10 (simulateCS (selectUntil @3 @C.System @(Int, Bool) P.snd) (dats, indices))
-- [(10,False),(20,False),(30,True),(40,True),(11,False),(21,True),(12,False),(22,False),(32,False),(42,True)]
--
selectUntil ::
  forall n dom a.
  C.KnownNat n =>
  (a -> Bool) ->
  Circuit
    (C.Vec n (Dfs dom a), Dfs dom (C.Index n))
    (Dfs dom a)
selectUntil f = Circuit $
    B.first (B.first C.unbundle . C.unbundle) . C.unbundle
  . fmap go
  . C.bundle . B.first (C.bundle . B.first C.bundle)
 where
  nacks = C.repeat (Ack False)

  go ((dats, dat), ack)
    | Data i <- dat
    , Data d <- dats C.!! i
    = ((C.replace i ack nacks, if f d then coerce ack else Ack False), Data d)
    | otherwise
    = ((nacks, Ack False), NoData)

-- | Mealy machine acting on raw Dfs stream
mealy ::
  ( C.HiddenClockResetEnable dom
  , C.NFDataX s ) =>
  -- | Transition function
  ( s ->
    (Maybe i, Protocols.Ack) ->
    (s, (Protocols.Ack, Maybe o)) ) ->
  -- | Initial state
  s ->
  -- | Circuit analogous to mealy machine
  Circuit (Dfs dom i) (Dfs dom o)
mealy f = DfLike.mealy f'
 where
  f' s =
      -- Add "metadata" (empty tuple)
      T.second (T.second (fmap ((),)))
      -- Feed to function not taking metadata
    . f s
      -- Strip "metadata" (empty tuple)
    . T.first (fmap P.snd)

-- | Copy data of a single 'Dfs' stream to multiple. LHS will only receive
-- an acknowledgement when all RHS receivers have acknowledged data.
fanout ::
  forall n dom a .
  (C.KnownNat n, C.HiddenClockResetEnable dom, 1 <= n) =>
  Circuit (Dfs dom a) (C.Vec n (Dfs dom a))
fanout = DfLike.fanout

-- | Merge data of multiple 'Dfs' streams using a user supplied function
fanin ::
  forall n dom a .
  (C.KnownNat n, 1 <= n) =>
  (a -> a -> a) ->
  Circuit (C.Vec n (Dfs dom a)) (Dfs dom a)
fanin f = bundleVec |> map (C.fold @_ @(n-1) f)

-- | Merge data of multiple 'Dfs' streams using Monoid's '<>'.
mfanin ::
  forall n dom a .
  (C.KnownNat n, Monoid a, 1 <= n) =>
  Circuit (C.Vec n (Dfs dom a)) (Dfs dom a)
mfanin = fanin (<>)

-- | Bundle a vector of 'Dfs' streams into one.
bundleVec ::
  forall n dom a .
  (C.KnownNat n, 1 <= n) =>
  Circuit (C.Vec n (Dfs dom a)) (Dfs dom (C.Vec n a))
bundleVec =
  Circuit (T.first C.unbundle . C.unbundle . fmap go . C.bundle . T.first C.bundle)
 where
  go (iDats, ack) = (acks, dat)
   where
    acks = C.repeat (bool (Ack False) (coerce ack) (isData dat))
    dat = sequence iDats

-- | Split up a 'Dfs' stream of a vector into multiple independent 'Dfs' streams.
unbundleVec ::
  forall n dom a .
  (C.KnownNat n, C.NFDataX a, C.HiddenClockResetEnable dom, 1 <= n) =>
  Circuit (Dfs dom (C.Vec n a)) (C.Vec n (Dfs dom a))
unbundleVec =
  Circuit (T.second C.unbundle . C.mealyB go initState . T.second C.bundle)
 where
  initState :: C.Vec n Bool
  initState = C.repeat False

  go ::
    C.Vec n Bool ->
    (Data (C.Vec n a), C.Vec n (Ack a)) ->
    (C.Vec n Bool, (Ack (C.Vec n a), C.Vec n (Data a)))
  go _acked (NoData, _) = (initState, (Ack False, C.repeat NoData))
  go acked (Data dataVec, acks) =
    let
      -- Send data to "clients" that have not acked yet
      valids_ = C.map not acked
      dats = C.zipWith (\d -> bool NoData (Data d)) dataVec valids_

      -- Store new acks, send ack if all "clients" have acked
      acked1 = C.zipWith (||) acked (coerce acks)
      ack = C.fold @_ @(n-1) (&&) acked1
    in
      ( if ack then initState else acked1
      , (coerce ack, dats) )

-- | Distribute data across multiple components on the RHS. Useful if you want
-- to parallelize a workload across multiple (slow) workers. For optimal
-- throughput, you should make sure workers can accept data every /n/ cycles.
roundrobin ::
  forall n dom a .
  (C.KnownNat n, C.HiddenClockResetEnable dom, 1 <= n) =>
  Circuit (Dfs dom a) (C.Vec n (Dfs dom a))
roundrobin =
  Circuit (T.second C.unbundle . C.mealyB go minBound . T.second C.bundle)
 where
  go :: C.Index n -> (Data a, C.Vec n (Ack a)) -> (C.Index n, (Ack a, C.Vec n (Data a)))
  go i (NoData, _) = (i, (Ack False, C.repeat NoData))
  go i0 (datIn, acks) = (i1, (Ack ack, datOut))
   where
    datOut = C.replace i0 datIn (C.repeat NoData)
    i1 = if ack then C.satSucc C.SatWrap i0 else i0
    Ack ack = acks !! i0

-- | Collect mode in 'roundrobinCollect'
data CollectMode
  -- | Collect in a /roundrobin/ fashion. If a component does not produce
  -- data, wait until it does.
  = NoSkip
  -- | Collect in a /roundrobin/ fashion. If a component does not produce
  -- data, skip it and check the next component on the next cycle.
  | Skip
  -- | Check all components in parallel. Biased towards the /last/ Dfs
  -- channel.
  | Parallel

-- | Opposite of 'roundrobin'. Useful to collect data from workers that only
-- produce a result with an interval of /n/ cycles.
roundrobinCollect ::
  forall n dom a .
  (C.KnownNat n, C.HiddenClockResetEnable dom, 1 <= n) =>
  CollectMode ->
  Circuit (C.Vec n (Dfs dom a)) (Dfs dom a)
roundrobinCollect NoSkip =
  Circuit (T.first C.unbundle . C.mealyB go minBound . T.first C.bundle)
 where
  go :: C.Index n -> (C.Vec n (Data a), Ack a) -> (C.Index n, (C.Vec n (Ack a), Data a))
  go i ((!!i) -> dat@(Data _), ~(Ack ack)) =
    ( if ack then C.satSucc C.SatWrap i else i
    , (C.replace i (Ack ack) (C.repeat (Ack False)), dat) )
  go i _ = (i, (C.repeat (Ack False), NoData))

roundrobinCollect Skip =
  Circuit (T.first C.unbundle . C.mealyB go minBound . T.first C.bundle)
 where
  go :: C.Index n -> (C.Vec n (Data a), Ack a) -> (C.Index n, (C.Vec n (Ack a), Data a))
  go i ((!!i) -> dat@(Data _), ~(Ack ack)) =
    ( if ack then C.satSucc C.SatWrap i else i
    , (C.replace i (Ack ack) (C.repeat (Ack False)), dat) )
  go i _ = (C.satSucc C.SatWrap i, (C.repeat (Ack False), NoData))

roundrobinCollect Parallel =
  Circuit (T.first C.unbundle . C.unbundle . fmap go . C.bundle . T.first C.bundle)
 where
  go :: (C.Vec n (Data a), Ack a) -> (C.Vec n (Ack a), Data a)
  go (dats0, ack) = (acks, dat)
   where
    nacks = C.repeat (Ack False)
    acks = fromMaybe nacks ((\i -> C.replace i ack nacks) <$> dataToMaybe iM)
    dats1 = C.zipWith (\i -> fmap (i,)) C.indicesI dats0
    (iM, dat) = Data.List.NonEmpty.unzip (C.fold @_ @(n-1) (<|>) dats1)

-- | Place register on /forward/ part of a circuit.
registerFwd ::
  forall dom a .
  (C.NFDataX a, C.HiddenClockResetEnable dom) =>
  Circuit (Dfs dom a) (Dfs dom a)
registerFwd = DfLike.registerFwd

-- | Place register on /backward/ part of a circuit. This is implemented using a
-- in-logic two-element shift register.
registerBwd ::
  (C.NFDataX a, C.HiddenClockResetEnable dom) =>
  Circuit (Dfs dom a) (Dfs dom a)
registerBwd = DfLike.registerBwd

--------------------------------- SIMULATE -------------------------------------

-- | Emit values given in list. Emits no data while reset is asserted. Not
-- synthesizable.
drive ::
  forall dom a.
  C.KnownDomain dom =>
  SimulationConfig ->
  [Data a] ->
  Circuit () (Dfs dom a)
drive conf s0 =
  Df.drive conf (P.map toDfData s0) |> DfLike.fromDf
 where
  toDfData :: Data a -> Df.Data () a
  toDfData NoData = Df.NoData
  toDfData (Data a) = Df.Data () a

-- | Sample protocol to a list of values. Drops values while reset is asserted.
-- Not synthesizable.
--
-- For a generalized version of 'sample', check out 'sampleC'.
sample ::
  forall dom b.
  C.KnownDomain dom =>
  SimulationConfig ->
  Circuit () (Dfs dom b) ->
  [Data b]
sample conf c =
  P.map fromDfData (Df.sample conf (c |> DfLike.toDf))
 where
  fromDfData :: Df.Data () b -> Data b
  fromDfData Df.NoData = NoData
  fromDfData (Df.Data _ a) = Data a

-- | Stall every valid Dfs packet with a given number of cycles. If there are
-- more valid packets than given numbers, passthrough all valid packets without
-- stalling. Not synthesizable.
--
-- For a generalized version of 'stall', check out 'stallC'.
stall ::
  forall dom a.
  ( C.KnownDomain dom
  , HasCallStack ) =>
  SimulationConfig ->
  -- | Acknowledgement to send when LHS does not send data. Stall will act
  -- transparently when reset is asserted.
  StallAck ->
  -- Number of cycles to stall for every valid Df packet
  [Int] ->
  Circuit (Dfs dom a) (Dfs dom a)
stall conf stallAck stalls =
  DfLike.toDf |> Df.stall conf stallAck stalls |> DfLike.fromDf

-- | Simulate a single domain protocol. Not synthesizable.
--
-- For a generalized version of 'simulate', check out 'Protocols.simulateC'.
simulate ::
  forall dom a b.
  C.KnownDomain dom =>
  -- | Simulation configuration. Use 'Data.Default.def' for sensible defaults.
  SimulationConfig ->
  -- | Circuit to simulate.
  ( C.Clock dom ->
    C.Reset dom ->
    C.Enable dom ->
    Circuit (Dfs dom a) (Dfs dom b) ) ->
  -- | Inputs
  [Data a] ->
  -- | Outputs
  [Data b]
simulate conf circ inputs =
    P.map fromDfData
  $ Df.simulate conf circDf
  $ P.map toDfData inputs
 where
  circDf clk rst ena = DfLike.fromDf |> circ clk rst ena |> DfLike.toDf

  toDfData :: Data a -> Df.Data () a
  toDfData NoData = Df.NoData
  toDfData (Data a) = Df.Data () a

  fromDfData :: Df.Data () b -> Data b
  fromDfData Df.NoData = NoData
  fromDfData (Df.Data _ a) = Data a
