{-|
Defines WriteResponse channel of full AXI4 protocol with port names corresponding
to the AXI4 specification.
-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE UndecidableInstances #-}

{-# OPTIONS_GHC -Wno-missing-fields #-}

module Protocols.Axi4.WriteResponse
  ( M2S_WriteResponse(..)
  , S2M_WriteResponse(..)
  , Axi4WriteResponse
  , mapFull
  ) where

-- base
import Data.Coerce (coerce)
import Data.Kind (Type)
import GHC.Generics (Generic)
import Data.Proxy

-- clash-prelude
import qualified Clash.Prelude as C

-- me
import Protocols.Axi4.Common
import Protocols.Internal
import Protocols.DfLike (DfLike)
import qualified Protocols.DfLike as DfLike

-- | AXI4 Read Data channel protocol
data Axi4WriteResponse
  (dom :: C.Domain)
  (kr :: KeepResponse)
  (iw :: IdWidth)
  (userType :: Type)

instance Protocol (Axi4WriteResponse dom kr iw userType) where
  type Fwd (Axi4WriteResponse dom kr iw userType) =
    C.Signal dom (S2M_WriteResponse kr iw userType)
  type Bwd (Axi4WriteResponse dom kr iw userType) =
    C.Signal dom M2S_WriteResponse

instance Backpressure (Axi4WriteResponse dom kr iw userType) where
  boolsToBwd _ = C.fromList_lazy . coerce

instance DfLike dom (Axi4WriteResponse dom kr iw) userType where
  type Data (Axi4WriteResponse dom kr iw) userType =
    S2M_WriteResponse kr iw userType

  type Payload userType = userType

  type Ack (Axi4WriteResponse dom kr iw) userType =
    M2S_WriteResponse

  getPayload _ (S2M_WriteResponse{_buser}) = Just _buser
  getPayload _ _ = Nothing
  {-# INLINE getPayload #-}

  setPayload _ _ dat (Just b) = dat{_buser=b}
  setPayload _ dfB _ Nothing = DfLike.noData dfB
  {-# INLINE setPayload #-}

  noData _ = S2M_NoWriteResponse
  {-# INLINE noData #-}

  boolToAck _ = coerce
  {-# INLINE boolToAck #-}

  ackToBool _ = coerce
  {-# INLINE ackToBool #-}

instance (C.KnownDomain dom, C.NFDataX userType, C.ShowX userType, Show userType) =>
  Simulate (Axi4WriteResponse dom kr iw userType) where

  type SimulateFwdType (Axi4WriteResponse dom kr iw userType) =
    [S2M_WriteResponse kr iw userType]

  type SimulateBwdType (Axi4WriteResponse dom kr iw userType) =
    [M2S_WriteResponse]

  type SimulateChannels (Axi4WriteResponse dom kr iw userType) = 1

  simToSigFwd Proxy = C.fromList_lazy
  simToSigBwd Proxy = C.fromList_lazy
  sigToSimFwd Proxy = C.sample_lazy
  sigToSimBwd Proxy = C.sample_lazy

  stallC conf (C.head -> (stallAck, stalls)) =
    DfLike.stall Proxy conf stallAck stalls

-- | See Table A2-4 "Write response channel signals"
data S2M_WriteResponse
  (kr :: KeepResponse)
  (iw :: IdWidth)
  (userType :: Type)
  = S2M_NoWriteResponse
  | S2M_WriteResponse
    { -- | Response ID
      _bid :: !(C.BitVector (Width iw))

      -- | Write response
    , _bresp :: !(ResponseType kr)

      -- | User data
    , _buser :: !userType
    }
  deriving (Generic)

-- | See Table A2-4 "Write response channel signals"
newtype M2S_WriteResponse = M2S_WriteResponse { _bready :: Bool }
  deriving (Show, Generic, C.NFDataX)

deriving instance
  ( C.NFDataX userType
  , C.NFDataX (ResponseType kr)
  , C.KnownNat (Width iw) ) =>
  C.NFDataX (S2M_WriteResponse kr iw userType)

deriving instance
  ( Show userType
  , Show (ResponseType kr)
  , C.KnownNat (Width iw) ) =>
  Show (S2M_WriteResponse kr iw userType)

-- | Circuit that transforms the LHS 'Axi4WriteResponse' protocol to a
-- version using different type parameters according to two functions
-- that can transform the data and ack signal to and from the other protocol.
mapFull ::
  forall dom
    kr1 iw1 userType1
    kr2 iw2 userType2 .
  (S2M_WriteResponse kr1 iw1 userType1 -> S2M_WriteResponse kr2 iw2 userType2) ->
  (M2S_WriteResponse -> M2S_WriteResponse) ->
  Circuit
    (Axi4WriteResponse dom kr1 iw1 userType1)
    (Axi4WriteResponse dom kr2 iw2 userType2)
mapFull = DfLike.mapDfLike Proxy Proxy
