{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE RecordWildCards #-}
{-|
Defines datatypes for all five channels of the AXI4 Lite protocol. For more
information on AXI4 Lite, see chapter B of the AMBA AXI specification.
-}

module Protocols.Axi4.Lite.Axi4Lite where

import Protocols
import Protocols.Axi4.Common
import Clash.Prelude as C

import Control.DeepSeq

-- | AXI4 Lite busses are always either 32 bit or 64 bit.
data BusWidth = Width32 | Width64 deriving (Show, Eq, Generic, NFDataX)

type instance Width 'Width32 = 32
type instance Width 'Width64 = 64

-- | AXI4 Lite defines a strobe signal to signify which bytes of the input
-- signal should be committed to memory. The strobe signal is encoded in
-- the 'Maybe' data type. Strobing is mandatory in AXI4 Lite.
type family WriteBusWidthType (bw :: BusWidth) where
  WriteBusWidthType 'Width32 = C.Vec 4 (Maybe (C.BitVector 8))
  WriteBusWidthType 'Width64 = C.Vec 8 (Maybe (C.BitVector 8))

-- | Type family mapping the two available bus widths to vectors of bytes.
type family ReadBusWidthType (bw :: BusWidth) where
  ReadBusWidthType 'Width32 = C.Vec 4 (C.BitVector 8)
  ReadBusWidthType 'Width64 = C.Vec 8 (C.BitVector 8)

---------------------------
--- Write address types ---
---------------------------


-- | The xvalid signals in AXI4 Lite are encoded in the datatype by having two
-- options, e.g. M2S_NoWriteAddress and M2S_WriteAddress. The rest of the channels
-- are fields in the record. Table B1.1 defines which signals AXI4 Lite uses.
data M2S_WriteAddress
  (aw :: AddrWidth)
  = M2S_NoWriteAddress
  | M2S_WriteAddress {
    -- | Address to be written to
    _awaddr :: !(C.BitVector (Width aw)),

    -- | Protection permissions, in AXI4 Lite these are always enabled.
    _awprot :: PermissionsType 'KeepPermissions
  } deriving (Generic, NFData)

deriving instance (C.KnownNat (Width aw))
  => Show (M2S_WriteAddress aw)

deriving instance (C.KnownNat (Width aw))
  => ShowX (M2S_WriteAddress aw)

deriving instance (C.KnownNat (Width aw))
  => Eq (M2S_WriteAddress aw)

deriving instance (C.KnownNat (Width aw))
  => NFDataX (M2S_WriteAddress aw)

-- | Ready signal for the write address channel.
data S2M_WriteAddress
  = S2M_WriteAddress {
    _awready :: Bool
  } deriving (Show, Generic, NFDataX)


------------------------
--- Write data types ---
------------------------

-- | Data type for the write data channel.
data M2S_WriteData
  (bw :: BusWidth)
  = M2S_NoWriteData
  | M2S_WriteData {
    -- | Write data
    _wdata :: !(WriteBusWidthType bw)
  } deriving (Generic)

deriving instance
  (C.KnownNat (Width bw)
  , Show (WriteBusWidthType bw))
  => Show (M2S_WriteData bw)

deriving instance (NFDataX (WriteBusWidthType bw)) => NFDataX (M2S_WriteData bw)

-- | Ready signal for the write data channel.
data S2M_WriteData
  = S2M_WriteData {
    _wready :: Bool
  } deriving (Show, Generic, NFDataX)

----------------------------
--- Write response types ---
----------------------------

-- | Data type for the write response channel. Notice that here the ready signal
-- goes from master to slave instead of the other way around.
data M2S_WriteResponse
  = M2S_WriteResponse {
    _bready :: Bool
  } deriving (Show, Generic, NFDataX)

-- | Data type for the write response channel from slave to master. On this channel
-- the response as defined in A3.4.4 is sent.
data S2M_WriteResponse
  = S2M_NoWriteResponse
  | S2M_WriteResponse {
    _bresp :: RespLite
  } deriving (Show, Generic, NFDataX)


--------------------------
--- Read address types ---
--------------------------

-- | Data type for the read address channel.
data M2S_ReadAddress
  (aw :: AddrWidth)
  = M2S_NoReadAddress
  | M2S_ReadAddress {
    _araddr :: !(C.BitVector (Width aw)),
    _arprot :: PermissionsType 'KeepPermissions
  } deriving (Generic)

deriving instance (KnownNat (Width aw)) => NFDataX (M2S_ReadAddress aw)

deriving instance
  (C.KnownNat (Width aw))
  => Show (M2S_ReadAddress aw)

-- | Ready signal for the read address channel.
data S2M_ReadAddress
  = S2M_ReadAddress {
    _arready :: Bool
  } deriving (Show, Generic, NFDataX)


-----------------------
--- Read data types ---
-----------------------

-- | Acknowledges data from the slave component. This data type needs the 'bw' type
-- to fullfil the injectivity requirement of 'Fwd' in 'Protocol', even though it only
-- contains a ready signal of type 'Bool'.
data M2S_ReadData
  (bw :: BusWidth)
  = M2S_ReadData {
    _rready :: Bool
  } deriving (Generic, NFDataX)

deriving instance (Show (ReadBusWidthType bw)) => Show (M2S_ReadData bw)

-- | Data type for the data sent over the read data channel from the slave to the master.
data S2M_ReadData
  (bw :: BusWidth)
  = S2M_NoReadData
  | S2M_ReadData {
    _rdata :: ReadBusWidthType bw,
    _rresp :: RespLite
  } deriving (Generic)

deriving instance (Show (ReadBusWidthType bw)) => Show (S2M_ReadData bw)
deriving instance (NFDataX (ReadBusWidthType bw)) => NFDataX (S2M_ReadData bw)


-- | Product type of the types of the five different channels in the direction of master to slave.
data M2S_Axi4Lite
  (aw :: AddrWidth)
  (bw :: BusWidth)
  = M2S_Axi4Lite {
    m2s_wa :: M2S_WriteAddress aw,
    m2s_wd :: M2S_WriteData bw,
    m2s_wr :: M2S_WriteResponse,
    m2s_ra :: M2S_ReadAddress aw,
    m2s_rd :: M2S_ReadData bw
  }
  deriving (Generic)

deriving instance
  (NFDataX (ReadBusWidthType bw), NFDataX (WriteBusWidthType bw), KnownNat (Width aw))
  => NFDataX (M2S_Axi4Lite aw bw)

deriving instance
  ( Show (ReadBusWidthType bw)
  , Show (WriteBusWidthType bw)
  , KnownNat (Width aw)
  , KnownNat (Width bw))
  => Show (M2S_Axi4Lite aw bw)

-- | Product type of the types of the five different channels in the direction of slave to master.
data S2M_Axi4Lite
  (aw :: AddrWidth)
  (bw :: BusWidth)
  = S2M_Axi4Lite {
    s2m_wa :: S2M_WriteAddress,
    s2m_wd :: S2M_WriteData,
    s2m_wr :: S2M_WriteResponse,
    s2m_ra :: S2M_ReadAddress,
    s2m_rd :: S2M_ReadData bw
  }
  deriving (Generic)

-- this breaks when e.g. fromList is used on an unconstrained value :: S2MAxi4Lite aw bw.
deriving instance (NFDataX (ReadBusWidthType bw)) => NFDataX (S2M_Axi4Lite aw bw)

deriving instance
  ( Show (ReadBusWidthType bw)
  , Show (WriteBusWidthType bw)
  , KnownNat (Width aw)
  , KnownNat (Width bw))
  => Show (S2M_Axi4Lite aw bw)

-- | Type for the full AXI4 Lite protocol.
data Axi4Lite
  (dom :: C.Domain)
  (aw :: AddrWidth)
  (bw :: BusWidth)

-- | Protocol instance for Axi4Lite. The protocol instance is a one signal of all the channels
-- in the AXI4 Lite protocol. Each channel type contains the data that is communicated over that channel.
instance Protocol (Axi4Lite dom aw bw) where
  type Fwd (Axi4Lite dom aw bw) = C.Signal dom (M2S_Axi4Lite aw bw)
  type Bwd (Axi4Lite dom aw bw) = C.Signal dom (S2M_Axi4Lite aw bw)


instance (C.KnownDomain dom, NFDataX (S2M_Axi4Lite aw bw)) => Simulate (Axi4Lite dom aw bw) where
  type SimulateFwdType (Axi4Lite dom aw bw) = [M2S_Axi4Lite aw bw]
  type SimulateBwdType (Axi4Lite dom aw bw) = [S2M_Axi4Lite aw bw]
  type SimulateChannels (Axi4Lite dom aw bw) = 5

  simToSigFwd _ = C.fromList_lazy
  simToSigBwd _ = C.fromList_lazy
  sigToSimFwd _ = C.sample_lazy
  sigToSimBwd _ = C.sample_lazy

  stallC SimulationConfig{..} (waStalls:>wdStalls:>wrStalls:>raStalls:>rdStalls:>Nil) = Circuit go
    where
      go (fwd, bwd) = (bwd, fwd)
