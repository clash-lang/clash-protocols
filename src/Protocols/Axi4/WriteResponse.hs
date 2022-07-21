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

    -- * configuration
  , Axi4WriteResponseConfig(..)
  , GoodAxi4WriteResponseConfig
  , BKeepResponse
  , BIdWidth
  ) where

-- base
import Data.Coerce (coerce)
import Data.Kind (Type)
import GHC.Generics (Generic)

-- clash-prelude
import qualified Clash.Prelude as C

-- me
import Protocols.Axi4.Common
import Protocols.Internal

-- | Configuration options for 'Axi4WriteResponse'.
data Axi4WriteResponseConfig = Axi4WriteResponseConfig
  { _bKeepResponse :: Bool
  , _bIdWidth      :: C.Nat
  }

-- | Grab '_bKeepResponse' from 'Axi4WriteResponseConfig' at the type level.
-- This boolean value determines whether to keep the '_bresp' field
-- in 'S2M_WriteResponse'.
type family BKeepResponse (conf :: Axi4WriteResponseConfig) where
  BKeepResponse ('Axi4WriteResponseConfig a _) = a

-- | Grab '_bIdWidth' from 'Axi4WriteResponseConfig' at the type level.
-- This nat value determines the size of the '_bid' field
-- in 'S2M_WriteResponse'.
type family BIdWidth (conf :: Axi4WriteResponseConfig) where
  BIdWidth ('Axi4WriteResponseConfig _ a) = a

-- | AXI4 Read Data channel protocol
data Axi4WriteResponse
  (dom :: C.Domain)
  (conf :: Axi4WriteResponseConfig)
  (userType :: Type)

instance Protocol (Axi4WriteResponse dom conf userType) where
  type Fwd (Axi4WriteResponse dom conf userType) =
    C.Signal dom (S2M_WriteResponse conf userType)
  type Bwd (Axi4WriteResponse dom conf userType) =
    C.Signal dom M2S_WriteResponse

instance Backpressure (Axi4WriteResponse dom conf userType) where
  boolsToBwd _ = C.fromList_lazy . coerce

-- | See Table A2-4 "Write response channel signals"
data S2M_WriteResponse
  (conf :: Axi4WriteResponseConfig)
  (userType :: Type)
  = S2M_NoWriteResponse
  | S2M_WriteResponse
    { -- | Response ID
      _bid :: !(C.BitVector (BIdWidth conf))

      -- | Write response
    , _bresp :: !(ResponseType (BKeepResponse conf))

      -- | User data
    , _buser :: !userType
    }
  deriving (Generic)

-- | See Table A2-4 "Write response channel signals"
newtype M2S_WriteResponse = M2S_WriteResponse { _bready :: Bool }
  deriving (Show, Generic, C.NFDataX)

-- | Shorthand for a "well-behaved" write response config,
-- so that we don't need to write out a bunch of type constraints later.
-- Holds for every configuration; don't worry about implementing this class.
class
  ( KeepTypeClass (BKeepResponse conf)
  , C.KnownNat (BIdWidth conf)
  , Show (ResponseType (BKeepResponse conf))
  , C.NFDataX (ResponseType (BKeepResponse conf))
  ) => GoodAxi4WriteResponseConfig conf

instance
  ( KeepTypeClass (BKeepResponse conf)
  , C.KnownNat (BIdWidth conf)
  , Show (ResponseType (BKeepResponse conf))
  , C.NFDataX (ResponseType (BKeepResponse conf))
  ) => GoodAxi4WriteResponseConfig conf

deriving instance
  ( GoodAxi4WriteResponseConfig conf
  , Show userType
  ) =>
  Show (S2M_WriteResponse conf userType)

deriving instance
  ( GoodAxi4WriteResponseConfig conf
  , C.NFDataX userType
  ) =>
  C.NFDataX (S2M_WriteResponse conf userType)
