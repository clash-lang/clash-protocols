{-|
Defines WriteData channel of full AXI4 protocol with port names corresponding
to the AXI4 specification.
-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE UndecidableInstances #-}

{-# OPTIONS_GHC -Wno-missing-fields #-}

module Protocols.Axi4.Partial.Full.WriteData
  ( M2S_WriteData(..)
  , S2M_WriteData(..)
  , Axi4WriteData
  , toStrict, toStrictDf
  , fromStrict, fromStrictDf
  ) where

-- base
import Data.Coerce (coerce)
import Data.Kind (Type)
import Data.Maybe (fromMaybe, isJust)
import Data.Proxy
import GHC.Generics (Generic)
import GHC.TypeNats (Nat)

-- clash-prelude
import qualified Clash.Prelude as C
import Clash.Prelude ((:::), type (*))

-- me
import Protocols.Axi4.Common
import Protocols.Internal
import Protocols.DfLike (DfLike)
import qualified Protocols.DfLike as DfLike
import qualified Protocols.Axi4.Strict.Full.WriteData as Strict
import qualified Protocols.Df as Df
import Protocols.Df (Df)

-- | AXI4 Write Data channel protocol
data Axi4WriteData
  (dom :: C.Domain)
  (ks :: KeepStrobe)
  (nBytes :: Nat)
  (userType :: Type)

instance Protocol (Axi4WriteData dom ks nBytes userType) where
  type Fwd (Axi4WriteData dom ks nBytes userType) =
    C.Signal dom (M2S_WriteData ks nBytes userType)
  type Bwd (Axi4WriteData dom ks nBytes userType) =
    C.Signal dom S2M_WriteData

instance Backpressure (Axi4WriteData dom ks dataType userType) where
  boolsToBwd _ = C.fromList_lazy . coerce

instance DfLike dom (Axi4WriteData dom ks dataType) userType where
  type Data (Axi4WriteData dom ks dataType) userType =
    M2S_WriteData ks dataType userType

  type Payload userType = userType

  type Ack (Axi4WriteData dom ks dataType) userType =
    S2M_WriteData

  getPayload _ (M2S_WriteData{_wvalid=True, _wuser}) = Just _wuser
  getPayload _ _ = Nothing
  {-# INLINE getPayload #-}

  setPayload _ _ dat (Just b) = dat{_wvalid=True, _wuser=b}
  setPayload _ dfB _ Nothing = DfLike.noData dfB
  {-# INLINE setPayload #-}

  noData _ = M2S_WriteData{_wvalid=False}
  {-# INLINE noData #-}

  boolToAck _ = coerce
  {-# INLINE boolToAck #-}

  ackToBool _ = coerce
  {-# INLINE ackToBool #-}

instance (C.KnownDomain dom, C.NFDataX userType, C.ShowX userType, Show userType) =>
  Simulate (Axi4WriteData dom ks nBytes userType) where

  type SimulateType (Axi4WriteData dom ks nBytes userType) =
    [M2S_WriteData ks nBytes userType]

  type ExpectType (Axi4WriteData dom ks nBytes userType) =
    [M2S_WriteData ks nBytes userType]

  type SimulateChannels (Axi4WriteData dom ks nBytes userType) = 1

  toSimulateType _ = id
  fromSimulateType _ = id

  driveC = DfLike.drive Proxy
  sampleC = DfLike.sample Proxy
  stallC conf (C.head -> (stallAck, stalls)) =
    DfLike.stall Proxy conf stallAck stalls

-- | See Table A2-3 "Write data channel signals"
data M2S_WriteData
  (ks :: KeepStrobe)
  (nBytes :: Nat)
  (userType :: Type) =
  M2S_WriteData
    { -- | Write data
      _wdata :: "WDATA" ::: C.BitVector (nBytes*8)

      -- | Write strobes
    , _wstrb :: "WSTRB" ::: StrobeType nBytes ks

      -- | Write last
    , _wlast :: "WLAST" ::: Bool

      -- | Write valid
    , _wvalid :: "WVALID" ::: Bool

      -- | User data
    , _wuser :: "WUSER" ::: userType
    }
  deriving (Generic)

-- | See Table A2-3 "Write data channel signals"
newtype S2M_WriteData = S2M_WriteData
  { _wready :: "WREADY" ::: Bool }
  deriving (Show, Generic, C.NFDataX)

deriving instance
  ( C.NFDataX userType
  , C.NFDataX (StrobeType nBytes ks) ) =>
  C.NFDataX (M2S_WriteData ks nBytes userType)

deriving instance
  ( Show userType
  , Show (StrobeType nBytes ks)
  , C.KnownNat nBytes ) =>
  Show (M2S_WriteData ks nBytes userType)

-- | Convert an 'Axi4WriteData' into its strict version
-- 'Strict.Axi4WriteData'. The latter is usually preferred in Clash designs
-- as its definitions don't contain partial fields. Note that the functions
-- defined over these circuits operate on @userType@. If you need functions to
-- map over all fields, consider using 'toStrictDf'.
toStrict ::
  C.KnownNat nBytes =>
  SKeepStrobe ks ->
  Circuit
    (Axi4WriteData dom ks nBytes userType)
    (Strict.Axi4WriteData dom ks nBytes userType)
toStrict SNoStrobe = Circuit (C.unbundle . fmap go . C.bundle)
 where
  go (M2S_WriteData{..}, ack)
    | _wvalid  = (coerce ack, Strict.M2S_WriteData{..})
    | otherwise = (coerce ack, Strict.M2S_NoWriteData)
toStrict SKeepStrobe = Circuit (C.unbundle . fmap go . C.bundle)
 where
  go (M2S_WriteData{..}, ack)
    | _wvalid  = (coerce ack, Strict.M2S_WriteData{_wdata=strobedData _wdata _wstrb,..})
    | otherwise = (coerce ack, Strict.M2S_NoWriteData)

  strobedData dat strb = C.zipWith orNothing (C.unpack strb) (C.unpack dat)
  orNothing p a = if p then Just a else Nothing

-- | Convert a 'Strict.Axi4WriteData' into 'Axi4WriteData'
fromStrict ::
  C.KnownNat nBytes =>
  SKeepStrobe ks ->
  Circuit
    (Strict.Axi4WriteData dom ks nBytes userType)
    (Axi4WriteData dom ks nBytes userType)
fromStrict SNoStrobe = Circuit (C.unbundle . fmap go . C.bundle)
 where
  go (Strict.M2S_WriteData{..}, ack) = (coerce ack, M2S_WriteData{_wvalid=True,..})
  go (_, ack) = (coerce ack, M2S_WriteData{_wvalid=False})
fromStrict SKeepStrobe = Circuit (C.unbundle . fmap go . C.bundle)
 where
  go (Strict.M2S_WriteData{..}, ack) =
    (coerce ack, M2S_WriteData
      { _wdata=C.pack (C.map (fromMaybe 0) _wdata)
      , _wstrb=C.pack (C.map isJust _wdata)
      , _wvalid=True
      , .. })
  go (_, ack) =
    (coerce ack, M2S_WriteData{_wvalid=False})

-- | Convert an 'Axi4WriteData' into its strict 'Df' equivalent.
toStrictDf ::
  C.KnownNat nBytes =>
  SKeepStrobe ks ->
  Circuit
    (Axi4WriteData dom ks nBytes userType)
    (Df dom (Strict.M2S_WriteData ks nBytes userType))
toStrictDf keepStrobe =
  toStrict keepStrobe |> Circuit (C.unbundle . fmap go . C.bundle)
 where
  go (dat@Strict.M2S_WriteData {}, ack) = (coerce ack, Df.Data dat)
  go (_, ack) = (coerce ack, Df.NoData)

-- | Convert an 'Axi4WriteData' into its strict 'Df' equivalent.
fromStrictDf ::
  C.KnownNat nBytes =>
  SKeepStrobe ks ->
  Circuit
    (Df dom (Strict.M2S_WriteData ks nBytes userType))
    (Axi4WriteData dom ks nBytes userType)
fromStrictDf keepStrobe =
   Circuit (C.unbundle . fmap go . C.bundle) |> fromStrict keepStrobe
 where
  go (Df.Data dat, ack) = (coerce ack, dat)
  go (_, ack) = (coerce ack, Strict.M2S_NoWriteData)
