{-|
Defines ReadData channel of full AXI4 protocol with port names corresponding
to the AXI4 specification.
-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE UndecidableInstances #-}

{-# OPTIONS_GHC -Wno-missing-fields #-}

module Protocols.Axi4.ReadData
  ( M2S_ReadData(..)
  , S2M_ReadData(..)
  , Axi4ReadData

    -- * configuration
  , Axi4ReadDataConfig(..)
  , GoodAxi4ReadDataConfig
  , RKeepResponse
  , RIdWidth
  ) where

-- base
import Data.Coerce (coerce)
import Data.Kind (Type)
import GHC.Generics (Generic)
import Prelude hiding
  ((!!), map, zip, zipWith, filter, fst, snd, either, const, pure)

-- clash-prelude
import qualified Clash.Prelude as C

-- me
import Protocols.Axi4.Common
import Protocols.Internal

-- | Configuration options for 'Axi4ReadData'.
data Axi4ReadDataConfig = Axi4ReadDataConfig
  { _rKeepResponse :: Bool
  , _rIdWidth      :: C.Nat
  }

-- | Grab '_rKeepResponse' from 'Axi4ReadDataConfig' at the type level.
-- This boolean value determines whether to keep the '_rresp' field
-- in 'S2M_ReadData'.
type family RKeepResponse (conf :: Axi4ReadDataConfig) where
  RKeepResponse ('Axi4ReadDataConfig a _) = a

-- | Grab '_rIdWidth' from 'Axi4ReadDataConfig' at the type level.
-- This nat value determines the size of the '_rid' field
-- in 'S2M_ReadData'.
type family RIdWidth (conf :: Axi4ReadDataConfig) where
  RIdWidth ('Axi4ReadDataConfig _ a) = a

-- | AXI4 Read Data channel protocol
data Axi4ReadData
  (dom :: C.Domain)
  (conf :: Axi4ReadDataConfig)
  (userType :: Type)
  (dataType :: Type)

instance Protocol (Axi4ReadData dom conf userType dataType) where
  type Fwd (Axi4ReadData dom conf userType dataType) =
    C.Signal dom (S2M_ReadData conf userType dataType)
  type Bwd (Axi4ReadData dom conf userType dataType) =
    C.Signal dom M2S_ReadData

instance Backpressure (Axi4ReadData dom conf userType dataType) where
  boolsToBwd _ = C.fromList_lazy . coerce

-- | See Table A2-6 "Read data channel signals"
data S2M_ReadData
  (conf :: Axi4ReadDataConfig)
  (userType :: Type)
  (dataType :: Type)
  = S2M_NoReadData
  | S2M_ReadData
    { -- | Read address id*
      _rid :: !(C.BitVector (RIdWidth conf))

    , -- | Read data
      _rdata :: !dataType

      -- | Read response
    , _rresp :: !(ResponseType (RKeepResponse conf))

      -- | Read last
    , _rlast :: !Bool

      -- | User data
    , _ruser :: !userType
    }
  deriving (Generic)

-- | See Table A2-6 "Read data channel signals"
newtype M2S_ReadData = M2S_ReadData { _rready :: Bool }
  deriving (Show, Generic, C.NFDataX)

-- | Shorthand for a "well-behaved" read data config,
-- so that we don't need to write out a bunch of type constraints later.
-- Holds for every configuration; don't worry about implementing this class.
class
  ( KeepTypeClass (RKeepResponse conf)
  , C.KnownNat (RIdWidth conf)
  , Show (ResponseType (RKeepResponse conf))
  , C.NFDataX (ResponseType (RKeepResponse conf))
  ) => GoodAxi4ReadDataConfig conf

instance
  ( KeepTypeClass (RKeepResponse conf)
  , C.KnownNat (RIdWidth conf)
  , Show (ResponseType (RKeepResponse conf))
  , C.NFDataX (ResponseType (RKeepResponse conf))
  ) => GoodAxi4ReadDataConfig conf

deriving instance
  ( GoodAxi4ReadDataConfig conf
  , Show userType
  , Show dataType
  ) =>
  Show (S2M_ReadData conf userType dataType)

deriving instance
  ( GoodAxi4ReadDataConfig conf
  , C.NFDataX userType
  , C.NFDataX dataType
  ) =>
  C.NFDataX (S2M_ReadData conf userType dataType)
