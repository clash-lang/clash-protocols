{-|
Defines WriteData channel of full AXI4 protocol with port names corresponding
to the AXI4 specification.
-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE UndecidableInstances #-}

{-# OPTIONS_GHC -Wno-missing-fields #-}

module Protocols.Axi4.WriteData
  ( M2S_WriteData(..)
  , S2M_WriteData(..)
  , Axi4WriteData
  , mapFull
  ) where

-- base
import Data.Coerce (coerce)
import Data.Kind (Type)
import GHC.Generics (Generic)
import GHC.TypeNats (Nat)
import Data.Proxy
import Prelude hiding
  ((!!), map, zip, zipWith, filter, fst, snd, either, const, pure)

-- clash-prelude
import qualified Clash.Prelude as C

-- me
import Protocols.Axi4.Common
import Protocols.Internal
import Protocols.DfLike (DfLike)
import qualified Protocols.DfLike as DfLike

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

  getPayload _ (M2S_WriteData{_wuser}) = Just _wuser
  getPayload _ _ = Nothing
  {-# INLINE getPayload #-}

  setPayload _ _ dat (Just b) = dat{_wuser=b}
  setPayload _ dfB _ Nothing = DfLike.noData dfB
  {-# INLINE setPayload #-}

  noData _ = M2S_NoWriteData
  {-# INLINE noData #-}

  boolToAck _ = coerce
  {-# INLINE boolToAck #-}

  ackToBool _ = coerce
  {-# INLINE ackToBool #-}

instance (C.KnownDomain dom, C.NFDataX userType, C.ShowX userType, Show userType) =>
  Simulate (Axi4WriteData dom ks nBytes userType) where

  type SimulateFwdType (Axi4WriteData dom ks nBytes userType) =
    [M2S_WriteData ks nBytes userType]

  type SimulateBwdType (Axi4WriteData dom ks nBytes userType) =
    [S2M_WriteData]

  type SimulateChannels (Axi4WriteData dom ks nBytes userType) = 1

  simToSigFwd Proxy = C.fromList_lazy
  simToSigBwd Proxy = C.fromList_lazy
  sigToSimFwd Proxy = C.sample_lazy
  sigToSimBwd Proxy = C.sample_lazy

  stallC conf (C.head -> (stallAck, stalls)) =
    DfLike.stall Proxy conf stallAck stalls

-- | See Table A2-3 "Write data channel signals". If strobing is kept, the data
-- will be a vector of 'Maybe' bytes. If strobing is not kept, data will be a
-- 'C.BitVector'.
data M2S_WriteData
  (ks :: KeepStrobe)
  (nBytes :: Nat)
  (userType :: Type)
  = M2S_NoWriteData
  | M2S_WriteData
    { -- | Write data
      _wdata :: !(StrictStrobeType nBytes ks)

      -- | Write last
    , _wlast :: !Bool

      -- | User data
    , _wuser :: !userType
    }
  deriving (Generic)

-- | See Table A2-3 "Write data channel signals"
newtype S2M_WriteData = S2M_WriteData { _wready :: Bool }
  deriving (Show, Generic, C.NFDataX)

deriving instance
  ( C.NFDataX userType
  , C.NFDataX (StrictStrobeType nBytes ks) ) =>
  C.NFDataX (M2S_WriteData ks nBytes userType)

deriving instance
  ( Show userType
  , Show (StrictStrobeType nBytes ks)
  , C.KnownNat nBytes ) =>
  Show (M2S_WriteData ks nBytes userType)

-- | Circuit that transforms the LHS 'Axi4WriteData' protocol to a
-- version using different type parameters according to two functions
-- that can transform the data and ack signal to and from the other protocol.
mapFull ::
  forall dom ks1 ks2 nBytes1 nBytes2 t1 t2.
  (M2S_WriteData ks1 nBytes1 t1 -> M2S_WriteData ks2 nBytes2 t2) ->
  (S2M_WriteData -> S2M_WriteData) ->
  Circuit ((Axi4WriteData dom ks1 nBytes1) t1) ((Axi4WriteData dom ks2 nBytes2) t2)
mapFull = DfLike.mapDfLike Proxy Proxy
