{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-missing-fields #-}

{- |
Defines WriteData channel of full AXI4 protocol with port names corresponding
to the AXI4 specification.
-}
module Protocols.Axi4.WriteData (
  M2S_WriteData (..),
  S2M_WriteData (..),
  Axi4WriteData,

  -- * configuration
  Axi4WriteDataConfig (..),
  KnownAxi4WriteDataConfig,
  WKeepStrobe,
  WNBytes,
) where

-- base
import Data.Coerce (coerce)
import Data.Kind (Type)
import GHC.Generics (Generic)
import Prelude hiding (
  const,
  either,
  filter,
  fst,
  map,
  pure,
  snd,
  zip,
  zipWith,
  (!!),
 )

-- clash-prelude
import qualified Clash.Prelude as C

-- me
import Protocols.Axi4.Common
import Protocols.Idle
import Protocols.Internal

-- | Configuration options for 'Axi4WriteData'.
data Axi4WriteDataConfig = Axi4WriteDataConfig
  { _wKeepStrobe :: Bool
  , _wNBytes :: C.Nat
  }

{- | Grab '_wKeepStrobe' from 'Axi4WriteDataConfig' at the type level.
This boolean value determines whether to keep strobe values in the '_wdata' field
in 'M2S_WriteData'.
-}
type family WKeepStrobe (conf :: Axi4WriteDataConfig) where
  WKeepStrobe ('Axi4WriteDataConfig a _) = a

{- | Grab '_wNBytes' from 'Axi4WriteDataConfig' at the type level.
This nat value determines the size of the '_wdata' field
in 'M2S_WriteData'.
-}
type family WNBytes (conf :: Axi4WriteDataConfig) where
  WNBytes ('Axi4WriteDataConfig _ a) = a

-- | AXI4 Write Data channel protocol
data
  Axi4WriteData
    (dom :: C.Domain)
    (conf :: Axi4WriteDataConfig)
    (userType :: Type)

instance Protocol (Axi4WriteData dom conf userType) where
  type
    Fwd (Axi4WriteData dom conf userType) =
      C.Signal dom (M2S_WriteData conf userType)
  type
    Bwd (Axi4WriteData dom conf userType) =
      C.Signal dom S2M_WriteData

instance Backpressure (Axi4WriteData dom conf userType) where
  boolsToBwd _ = C.fromList_lazy . coerce

{- | See Table A2-3 "Write data channel signals". If strobing is kept, the data
will be a vector of 'Maybe' bytes. If strobing is not kept, data will be a
'C.BitVector'.
-}
data
  M2S_WriteData
    (conf :: Axi4WriteDataConfig)
    (userType :: Type)
  = M2S_NoWriteData
  | M2S_WriteData
      { _wdata :: !(StrictStrobeType (WNBytes conf) (WKeepStrobe conf))
      -- ^ Write data
      , _wlast :: !Bool
      -- ^ Write last
      , _wuser :: !userType
      -- ^ User data
      }
  deriving (Generic)

-- | See Table A2-3 "Write data channel signals"
newtype S2M_WriteData = S2M_WriteData {_wready :: Bool}
  deriving stock (Show, Generic)
  deriving anyclass (C.NFDataX)

{- | Shorthand for a "well-behaved" write data config,
so that we don't need to write out a bunch of type constraints later.
Holds for every configuration; don't worry about implementing this class.
-}
type KnownAxi4WriteDataConfig conf =
  ( KeepStrobeClass (WKeepStrobe conf)
  , C.KnownNat (WNBytes conf)
  , Show (StrobeDataType (WKeepStrobe conf))
  , C.NFDataX (StrobeDataType (WKeepStrobe conf))
  )

deriving instance
  ( KnownAxi4WriteDataConfig conf
  , Show userType
  ) =>
  Show (M2S_WriteData conf userType)

deriving instance
  ( KnownAxi4WriteDataConfig conf
  , C.NFDataX userType
  ) =>
  C.NFDataX (M2S_WriteData conf userType)

instance IdleCircuit (Axi4WriteData dom conf userType) where
  idleFwd _ = C.pure M2S_NoWriteData
  idleBwd _ = C.pure S2M_WriteData{_wready = False}
