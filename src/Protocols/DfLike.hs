{-|
At their core a lot of Dataflow protocols are very similar to the one defined
in "Protocols.Df": they carry data (and possible metadata) on their forward
channel, and an acknowledgement on their backward channel. This module introduces
"DfLike" which abstracts over these different, yet similar, protocols.
-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Protocols.DfLike
  ( -- * DfLike class
    DfLike(Meta, Payload, fromDf, toDf)

    -- * Operations on Df like protocols
  , map, mapMeta
  , fst, snd
  , mapMaybe, catMaybes
  , filter, filterMeta
  , bimap
  , either
  , first, firstT, mapLeft
  , second, secondT, mapRight
  , const
  , pure
  , mealy
  , mealyV
  , fanout
  , registerFwd
  , registerBwd
  ) where

import           Protocols hiding (Ack(..))
import qualified Protocols
import qualified Protocols.Df as Df
import           Protocols.Df (Df)

import           Clash.Prelude (Domain, type (<=))
import qualified Clash.Prelude as C

import qualified Prelude as P
import           Prelude hiding (map, const, fst, snd, pure, either, filter)

import           Data.Bifunctor (Bifunctor)
import           Data.Kind (Type)

-- | Dataflow protocols that can be converted from and to "Protocols.Df.Df".
--
-- Laws:
--
--   1. @fromDf |> toDf@ ~ @idC@
--   2. @toDf |> fromDf@ ~ @idC@
--
class DfLike (dom :: Domain) (df :: Type) | df -> dom where
  -- | \"Interesting\" data carried by protocol. This is where most functions
  -- are defined over.
  type Payload df :: Type

  -- | Metadata carried by protocol. This will typically hold destination
  -- addresses and the like. Metadata is expected to not change that often,
  -- hence most convenience functions being defined over the /payload/ instead.
  type Meta df :: Type

  -- | Convert a Df stream into a Df like stream
  fromDf :: Circuit (Df dom (Meta df) (Payload df)) df

  -- | Convert a Df like stream into a Df stream
  toDf :: Circuit df (Df dom (Meta df) (Payload df))

instance DfLike dom (Df dom meta a) where
  type Payload (Df dom meta a)  = a
  type Meta (Df dom meta a) = meta

  fromDf = idC
  toDf = idC

-- | Convert a protocol over 'Df' to a protocol over a 'Df' like protocol
withDf ::
  ( DfLike dom a
  , DfLike dom b ) =>
  Circuit
    (Df dom (Meta a) (Payload a))
    (Df dom (Meta b) (Payload b))
  -> Circuit a b
withDf c = toDf |> c |> fromDf

-- | Like 'Data.Bifunctor.bimap', but over payload of a DF like stream. Concrete
-- DF like types should implement this with a more concrete type signature.
-- Examples: 'Df.bimap', 'Dfs.bimap'.
bimap ::
  ( DfLike dom a
  , DfLike dom b
  , Meta a ~ Meta b
  , (u, v) ~ Payload a
  , (w, x) ~ Payload b
  ) =>
  (u -> w) ->
  (v -> x) ->
  Circuit a b
bimap f g = withDf (Df.bimap f g)

-- | Like 'P.map', but over payload of a DF like stream. Concrete DF like types
-- should implement this with a more concrete type signature. Examples: 'Df.map',
-- 'Dfs.map'.
map ::
  ( DfLike dom a
  , DfLike dom b
  , Meta a ~ Meta b
  ) =>
  (Payload a -> Payload b) ->
  Circuit a b
map f = withDf (Df.map f)

-- | Like 'Data.Maybe.catMaybes', but over payload (/a/) of a like Df stream.
-- Concrete DF like types should implement this with a more concrete type
-- signature. Examples: 'Df.catMaybes', 'Dfs.catMaybes'.
catMaybes ::
  ( DfLike dom a
  , DfLike dom b
  , Meta a ~ Meta b
  , Payload a ~ Maybe x
  , Payload b ~ x
  ) =>
  Circuit a b
catMaybes = withDf Df.catMaybes

-- | Like 'Data.Maybe.mapMaybe', but over payload (/a/) of a like Df stream.
-- Concrete DF like types should implement this with a more concrete type
-- signature. Examples: 'Df.mapMaybe', 'Dfs.mapMaybe'.
mapMaybe ::
  ( DfLike dom a
  , DfLike dom b
  , Meta a ~ Meta b
  , Payload a ~ x
  , Payload b ~ y
  ) =>
  (x -> Maybe y) ->
  Circuit a b
mapMaybe f = withDf (Df.mapMaybe f)

-- | Like 'P.filter', but over payload (/a/) of a like Df stream. Concrete DF
-- like types should implement this with a more concrete type signature.
-- Examples: 'Df.filter', 'Dfs.filter'.
filter :: DfLike dom a => (Payload a -> Bool) -> Circuit a a
filter f = withDf (Df.filter f)

-- | Like 'P.filterMeta', but over metadata (/meta/) of a like Df stream. Concrete
-- DF like types should implement this with a more concrete type signature.
-- Examples: 'Df.filterMeta', 'Dfs.filterMeta'.
filterMeta :: DfLike dom a => (Meta a -> Bool) -> Circuit a a
filterMeta f = withDf (Df.filterMeta f)

-- | Like 'P.map', but over the metadata of a DF like stream. Concrete DF like
-- types should implement this with a more concrete type signature. Examples:
-- 'Df.mapMeta', 'Dfs.mapMeta'.
mapMeta ::
  ( DfLike dom a
  , DfLike dom b
  , Payload a ~ Payload b
  ) =>
  (Meta a -> Meta b) ->
  Circuit a b
mapMeta f = withDf (Df.mapMeta f)

-- | Acknowledge but ignore data from LHS protocol. Send a static value composed
-- of /meta/ and /payload/. Concrete Df like types should implement this with a
-- more concrete type signature. Examples: 'Df.const', 'Dfs.const'.
const ::
  ( DfLike dom a
  , DfLike dom b
  ) =>
  Meta b ->
  Payload b ->
  Circuit a b
const meta payload = withDf (Df.const meta payload)

-- | Drive a constant value composed of /meta/ and /payload/. Concrete Df like
-- types should implement this with a more concrete type signature. Examples:
-- 'Df.const', 'Dfs.const'.
pure :: DfLike dom a => Meta a -> Payload a -> Circuit () a
pure meta payload = Df.pure meta payload |> fromDf

-- | Like 'P.fst', but over payload of a DF like stream. Concrete DF like types
-- should implement this with a more concrete type signature. Examples: 'Df.fst',
-- 'Dfs.fst'.
fst ::
  ( DfLike dom a
  , DfLike dom b
  , Meta a ~ Meta b
  , Payload a ~ (x, z)
  , Payload b ~ x
  ) =>
  Circuit a b
fst = map P.fst

-- | Like 'P.snd', but over payload of a DF like stream. Concrete DF like types
-- should implement this with a more concrete type signature. Examples: 'Df.snd',
-- 'Dfs.snd'.
snd ::
  ( DfLike dom a
  , DfLike dom b
  , Meta a ~ Meta b
  , Payload a ~ (x, z)
  , Payload b ~ z
  ) =>
  Circuit a b
snd = map P.snd

-- | Like 'T.first', but over payload of a DF like stream. Concrete DF like types
-- should implement this with a more concrete type signature. Examples: 'Df.firstT',
-- 'Dfs.firstT'.
firstT ::
  ( DfLike dom a
  , DfLike dom b
  , Meta a ~ Meta b
  , Payload a ~ (x, z)
  , Payload b ~ (y, z)
  ) =>
  (x -> y) ->
  Circuit a b
firstT f = withDf (Df.firstT f)

-- | Like 'T.second', but over payload of a DF like stream. Concrete DF like types
-- should implement this with a more concrete type signature. Examples: 'Df.second',
-- 'Dfs.second'.
secondT ::
  ( DfLike dom a
  , DfLike dom b
  , Meta a ~ Meta b
  , Payload a ~ (x, y)
  , Payload b ~ (x, z)
  ) =>
  (y -> z) ->
  Circuit a b
secondT f = withDf (Df.secondT f)

-- | Like 'Data.Bifunctor.first', but over payload of a DF like stream. Concrete
-- DF like types should implement this with a more concrete type signature.
-- Examples: 'Df.first', 'Dfs.first'.
first ::
  ( DfLike dom a
  , DfLike dom b
  , Bifunctor p
  , Meta a ~ Meta b
  , Payload a ~ p x z
  , Payload b ~ p y z
  ) =>
  (x -> y) ->
  Circuit a b
first f = withDf (Df.first f)

-- | Like 'Data.Bifunctor.second', but over payload of a DF like stream. Concrete
-- DF like types should implement this with a more concrete type signature.
-- Examples: 'Df.second', 'Dfs.second'.
second ::
  ( DfLike dom a
  , DfLike dom b
  , Bifunctor p
  , Meta a ~ Meta b
  , Payload a ~ p x y
  , Payload b ~ p x z
  ) =>
  (y -> z) ->
  Circuit a b
second f = withDf (Df.second f)

-- | Like 'Data.Either.Combinators.mapLeft', but over payload of a DF like
-- stream. Concrete DF like types should implement this with a more concrete
-- type signature. Examples: 'Df.mapLeft', 'Dfs.mapLeft'.
mapLeft ::
  ( DfLike dom a
  , DfLike dom b
  , Meta a ~ Meta b
  , Payload a ~ Either x z
  , Payload b ~ Either y z
  ) =>
  (x -> y) ->
  Circuit a b
mapLeft f = withDf (Df.mapLeft f)

-- | Like 'Data.Either.Combinators.mapRight', but over payload of a DF like
-- stream. Concrete DF like types should implement this with a more concrete
-- type signature. Examples: 'Df.mapRight', 'Dfs.mapRight'.
mapRight ::
  ( DfLike dom a
  , DfLike dom b
  , Meta a ~ Meta b
  , Payload a ~ Either x y
  , Payload b ~ Either x z
  ) =>
  (y -> z) ->
  Circuit a b
mapRight f = withDf (Df.mapRight f)

-- | Like 'P.either', but over payload of a DF like stream. Concrete DF like
-- types should implement this with a more concrete type signature. Examples:
-- 'Df.either', 'Dfs.either'.
either ::
  ( DfLike dom a
  , DfLike dom b
  , Meta a ~ Meta b
  , Payload a ~ Either x y
  , Payload b ~ z
  ) =>
  (x -> z) ->
  (y -> z) ->
  Circuit a b
either f g = withDf (Df.either f g)

-- | Mealy machine acting on raw DF stream
mealy ::
  ( C.HiddenClockResetEnable dom
  , C.NFDataX s
  , DfLike dom a
  , DfLike dom b
  ) =>
  -- | Transition function
  ( s ->
    (Maybe (Meta a, Payload a), Protocols.Ack) ->
    (s, (Protocols.Ack, Maybe (Meta b, Payload b))) ) ->
  -- | Initial state
  s ->
  -- | Circuit analogous to mealy machine
  Circuit a b
mealy f s = withDf (Df.mealy f s)

-- | Mealy machine that only transitions on DF transactions
mealyV ::
  forall s a b dom .
  ( C.HiddenClockResetEnable dom
  , C.NFDataX s
  , DfLike dom a
  , DfLike dom b
  , Meta a ~ Meta b
  ) =>
  -- | Transition function
  (s -> Payload a -> (s, Maybe (Payload b))) ->
  -- | Initial state
  s ->
  -- | Circuit analogous to mealy machine
  Circuit a b
mealyV f s = withDf (Df.mealyV f s)

-- | Copy (meta)data of a single Df like stream to multiple. LHS will only
-- receive an acknowledgement when all RHS receivers have acknowledged data.
fanout ::
  forall n a dom.
  ( C.KnownNat n
  , C.HiddenClockResetEnable dom
  , 1 <= n
  , DfLike dom a
  ) =>
  Circuit a (C.Vec n a)
fanout =
      toDf
  |> Df.fanout
  |> repeatC fromDf

-- | Place register on /forward/ part of a circuit
registerFwd ::
  forall dom a .
  ( C.NFDataX (Meta a)
  , C.NFDataX (Payload a)
  , C.HiddenClockResetEnable dom
  , DfLike dom a
  ) =>
  Circuit a a
registerFwd =
      toDf
  |> Df.registerFwd
  |> fromDf

-- | Place register on /backward/ part of a circuit. This is implemented using a
-- in-logic two-element shift register.
registerBwd ::
  ( C.NFDataX (Meta a)
  , C.NFDataX (Payload a)
  , C.HiddenClockResetEnable dom
  , DfLike dom a
  ) =>
  Circuit a a
registerBwd =
      toDf
  |> Df.registerBwd
  |> fromDf
