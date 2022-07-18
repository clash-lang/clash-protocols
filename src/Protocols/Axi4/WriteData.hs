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

    -- * configuration
  , Axi4WriteDataConfig(..)
  , GoodAxi4WriteDataConfig
  , WKeepStrobe
  , WNBytes
  ) where

-- base
import Data.Coerce (coerce)
import Data.Kind (Type)
import GHC.Generics (Generic)
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

-- | Configuration options for 'Axi4WriteData'.
data Axi4WriteDataConfig = Axi4WriteDataConfig
  { _wKeepStrobe :: Bool
  , _wNBytes     :: C.Nat
  }

-- | Grab '_wKeepStrobe' from 'Axi4WriteDataConfig' at the type level.
-- This boolean value determines whether to keep strobe values in the '_wdata' field
-- in 'M2S_WriteData'.
type family WKeepStrobe (conf :: Axi4WriteDataConfig) where
  WKeepStrobe ('Axi4WriteDataConfig a _) = a

-- | Grab '_wNBytes' from 'Axi4WriteDataConfig' at the type level.
-- This nat value determines the size of the '_wdata' field
-- in 'M2S_WriteData'.
type family WNBytes (conf :: Axi4WriteDataConfig) where
  WNBytes ('Axi4WriteDataConfig _ a) = a

-- | AXI4 Write Data channel protocol
data Axi4WriteData
  (dom :: C.Domain)
  (conf :: Axi4WriteDataConfig)
  (userType :: Type)

instance Protocol (Axi4WriteData dom conf userType) where
  type Fwd (Axi4WriteData dom conf userType) =
    C.Signal dom (M2S_WriteData conf userType)
  type Bwd (Axi4WriteData dom conf userType) =
    C.Signal dom S2M_WriteData

instance Backpressure (Axi4WriteData dom conf userType) where
  boolsToBwd _ = C.fromList_lazy . coerce

instance DfLike dom (Axi4WriteData dom conf) userType where
  type Data (Axi4WriteData dom conf) userType =
    M2S_WriteData conf userType

  type Payload userType = userType

  type Ack (Axi4WriteData dom conf) userType =
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
  Simulate (Axi4WriteData dom conf userType) where

  type SimulateFwdType (Axi4WriteData dom conf userType) =
    [M2S_WriteData conf userType]

  type SimulateBwdType (Axi4WriteData dom conf userType) =
    [S2M_WriteData]

  type SimulateChannels (Axi4WriteData dom conf userType) = 1

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
  (conf :: Axi4WriteDataConfig)
  (userType :: Type)
  = M2S_NoWriteData
  | M2S_WriteData
    { -- | Write data
      _wdata :: !(StrictStrobeType (WNBytes conf) (WKeepStrobe conf))

      -- | Write last
    , _wlast :: !Bool

      -- | User data
    , _wuser :: !userType
    }
  deriving (Generic)

-- | See Table A2-3 "Write data channel signals"
newtype S2M_WriteData = S2M_WriteData { _wready :: Bool }
  deriving (Show, Generic, C.NFDataX)

-- | Shorthand for a "well-behaved" write data config,
-- so that we don't need to write out a bunch of type constraints later.
-- Holds for every configuration; don't worry about implementing this class.
class
  ( KeepStrobeClass (WKeepStrobe conf)
  , C.KnownNat (WNBytes conf)
  , Show (StrobeDataType (WKeepStrobe conf))
  , C.NFDataX (StrobeDataType (WKeepStrobe conf))
  ) => GoodAxi4WriteDataConfig conf

instance
  ( KeepStrobeClass (WKeepStrobe conf)
  , C.KnownNat (WNBytes conf)
  , Show (StrobeDataType (WKeepStrobe conf))
  , C.NFDataX (StrobeDataType (WKeepStrobe conf))
  ) => GoodAxi4WriteDataConfig conf

deriving instance
  ( GoodAxi4WriteDataConfig conf
  , Show userType
  ) =>
  Show (M2S_WriteData conf userType)

deriving instance
  ( GoodAxi4WriteDataConfig conf
  , C.NFDataX userType
  ) =>
  C.NFDataX (M2S_WriteData conf userType)

-- | Circuit that transforms the LHS 'Axi4WriteData' protocol to a
-- version using different type parameters according to two functions
-- that can transform the data and ack signal to and from the other protocol.
mapFull ::
  forall dom conf1 conf2 t1 t2.
  (M2S_WriteData conf1 t1 -> M2S_WriteData conf2 t2) ->
  (S2M_WriteData -> S2M_WriteData) ->
  Circuit ((Axi4WriteData dom conf1) t1) ((Axi4WriteData dom conf2) t2)
mapFull = DfLike.mapDfLike Proxy Proxy
