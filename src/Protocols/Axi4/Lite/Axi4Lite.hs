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
import Clash.Signal.Internal
import Data.List ((\\))

import Control.DeepSeq

import qualified Data.Bifunctor as B



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

-- | Acknowledges data from the slave component.
data M2S_ReadData
  = M2S_ReadData {
    _rready :: Bool
  } deriving (Generic, NFDataX, Show)


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
    m2s_rd :: M2S_ReadData
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

  stallC conf stallAckVector = Circuit go
    where
      (waStalls:>wdStalls:>wrStalls:>raStalls:>rdStalls:>Nil) = stallAckVector
      SimulationConfig{..} = conf
      go :: (Signal dom (M2S_Axi4Lite aw bw),
             Signal dom (S2M_Axi4Lite aw bw)) ->
            (Signal dom (S2M_Axi4Lite aw bw),
             Signal dom (M2S_Axi4Lite aw bw))
      go (fwd, bwd) = (bwd', fwd')
        where
          bwd' = S2M_Axi4Lite <$> waBwdOut <*> wdBwdOut <*> wrBwdOut <*> raBwdOut <*> rdBwdOut
          fwd' = M2S_Axi4Lite <$> waFwdOut <*> wdFwdOut <*> wrFwdOut <*> raFwdOut <*> rdFwdOut

          (waFwd, wdFwd, wrFwd, raFwd, rdFwd) = dissectM2S fwd
          (waBwd, wdBwd, wrBwd, raBwd, rdBwd) = dissectS2M bwd

          (waStallAck, waStallNs) = waStalls
          (waBwdOut, waFwdOut) = stallM2S (stallAcks waStallAck) waStallNs resetCycles waFwd waBwd

          (wdStallAck, wdStallNs) = wdStalls
          (wdBwdOut, wdFwdOut) = stallM2S (stallAcks wdStallAck) wdStallNs resetCycles wdFwd wdBwd

          (wrStallAck, wrStallNs) = wrStalls
          (wrBwdOut, wrFwdOut) = stallS2M (stallAcks wrStallAck) wrStallNs resetCycles wrFwd wrBwd

          (raStallAck, raStallNs) = raStalls
          (raBwdOut, raFwdOut) = stallM2S (stallAcks raStallAck) raStallNs resetCycles raFwd raBwd

          (rdStallAck, rdStallNs) = rdStalls
          (rdBwdOut, rdFwdOut) = stallS2M (stallAcks rdStallAck) rdStallNs resetCycles rdFwd rdBwd

      dissectM2S (m :- m2s) =
        ( m2s_wa m :- waSig
        , m2s_wd m :- wdSig
        , m2s_wr m :- wrSig
        , m2s_ra m :- raSig
        , m2s_rd m :- rdSig )
        where
          (waSig, wdSig, wrSig, raSig, rdSig) = dissectM2S m2s

      dissectS2M (s :- s2m) =
        ( s2m_wa s :- waSig
        , s2m_wd s :- wdSig
        , s2m_wr s :- wrSig
        , s2m_ra s :- raSig
        , s2m_rd s :- rdSig )
        where
          (waSig, wdSig, wrSig, raSig, rdSig) = dissectS2M s2m


      stallAcks stallAck = cycle saList
        where
          saList | stallAck == StallCycle = [minBound..maxBound] \\ [StallCycle]
                 | otherwise = [stallAck]


      stallM2S :: (Source src, Destination dst) =>
        [StallAck] -> [Int] -> Int ->
        Signal dom src -> Signal dom dst ->
        (Signal dom dst, Signal dom src)
      stallM2S [] _ _ _ _ = error "finite stallAck list"
      stallM2S sas stalls resetN (f :- fwd) (b :- bwd) | resetN > 0 =
        B.bimap (b :-) (f :-) (stallM2S sas stalls (resetN - 1) fwd bwd)
      stallM2S (sa:sas) [] _ (f :- fwd) ~(b :- bwd) =
        B.bimap (toStallAck (maybePayload f) (isReady b) sa :-) (f :-) (stallM2S sas [] 0 fwd bwd)
      stallM2S (sa:sas) stalls _ ((maybePayload -> Nothing) :- fwd) ~(b :- bwd) =
        B.bimap (b' :-) (noData :-) (stallM2S sas stalls 0 fwd bwd)
          where b' = toStallAck (Nothing :: Maybe (M2S_WriteAddress aw)) (isReady b) sa
      stallM2S (_:sas) (stall:stalls) _ (f0 :- fwd) ~(b0 :- bwd) =
        let
          (f1, b1, stalls') = case compare 0 stall of
            LT -> (noData, ready False, (stall - 1):stalls)
            EQ -> (f0, b0, if isReady b0 then stalls else stall:stalls)
            GT -> error ("Unexpected negative stall: " <> show stall)
        in
          B.bimap (b1 :-) (f1 :-) (stallM2S sas stalls' 0 fwd bwd)

      stallS2M :: (Destination dst, Source src) =>
        [StallAck] -> [Int] -> Int ->
        Signal dom dst -> Signal dom src ->
        (Signal dom src, Signal dom dst)
      stallS2M sas stalls resetN fwd bwd = (src, dst)
        where (dst, src) = stallM2S sas stalls resetN bwd fwd

      toStallAck :: (Source src, Destination dst) => Maybe src -> Bool -> StallAck -> dst
      toStallAck (Just _) ack = const (ready ack)
      toStallAck Nothing ack = \case
        StallWithNack -> ready False
        StallWithAck -> ready True
        StallWithErrorX -> C.errorX "No defined ack"
        StallTransparently -> ready ack
        StallCycle -> ready False -- shouldn't happen..

-- | Every data-carrying direction in a channel in AXI4 has a @<Channel>@ and @No<Channel>@
-- constructor. In some functions (like "stallC") it is useful to write functions that
-- use this fact such that these can be applied to every channel of AXI4. This typeclass
-- provides functions to convert a value in a channel to @Maybe@, where the @No<Channel>@ is
-- converted to @Nothing@, and any other value to @Just value@.
--
-- This class is called @Source@ because a source of useful data is the sender of the type
-- for which this class is relevant.
class Source src where
  -- | Converts a channel type to a @Maybe@
  maybePayload :: src -> Maybe src
  -- | The value of type "src" that is mapped to @Nothing@ by "maybePayload"
  noData :: src

-- | Typeclass to convert Booleans to channel-specific acknowledgement types.
class Destination dst where
  ready :: Bool -> dst
  isReady :: dst -> Bool

instance Source (M2S_WriteAddress aw) where
  maybePayload M2S_NoWriteAddress = Nothing
  maybePayload m2s_wa = Just m2s_wa

  noData = M2S_NoWriteAddress

instance Destination S2M_WriteAddress where
  ready b = S2M_WriteAddress b
  isReady (S2M_WriteAddress b) = b

instance Source (M2S_WriteData bw) where
  maybePayload M2S_NoWriteData = Nothing
  maybePayload m2s_wd = Just m2s_wd

  noData = M2S_NoWriteData

instance Destination S2M_WriteData where
  ready b = S2M_WriteData b
  isReady (S2M_WriteData b) = b

instance Source (S2M_WriteResponse) where
  maybePayload S2M_NoWriteResponse = Nothing
  maybePayload s2m_wr = Just s2m_wr

  noData = S2M_NoWriteResponse

instance Destination (M2S_WriteResponse) where
  ready b = M2S_WriteResponse b
  isReady (M2S_WriteResponse b) = b

instance Source (M2S_ReadAddress aw) where
  maybePayload M2S_NoReadAddress = Nothing
  maybePayload m2s_ra = Just m2s_ra

  noData = M2S_NoReadAddress

instance Destination S2M_ReadAddress where
  ready b = S2M_ReadAddress b
  isReady (S2M_ReadAddress b) = b

instance Source (S2M_ReadData bw) where
  maybePayload S2M_NoReadData = Nothing
  maybePayload s2m_rd = Just s2m_rd

  noData = S2M_NoReadData

instance Destination M2S_ReadData where
  ready b = M2S_ReadData b
  isReady (M2S_ReadData b) = b
