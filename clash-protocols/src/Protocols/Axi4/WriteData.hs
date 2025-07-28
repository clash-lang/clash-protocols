{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-missing-fields #-}
{-# OPTIONS_GHC -fconstraint-solver-iterations=10 #-}

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

  -- * write data info
  Axi4WriteDataInfo (..),
  axi4WriteDataMsgToWriteDataInfo,
  axi4WriteDataMsgFromWriteDataInfo,
  forceResetSanity,
) where

-- base
import Control.DeepSeq (NFData)
import Data.Bifunctor qualified as B
import Data.Coerce (coerce)
import Data.Kind (Type)
import Data.Proxy (Proxy (..))
import GHC.Generics (Generic)

-- clash-prelude
import Clash.Prelude (bundle, unbundle)
import Clash.Prelude qualified as C

-- me
import Protocols.Axi4.Common
import Protocols.Df qualified as Df
import Protocols.DfConv qualified as DfConv
import Protocols.Hedgehog (Test (..))
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

type IsLast = Bool

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
      { _wdata :: StrictStrobeType (WNBytes conf) (WKeepStrobe conf)
      -- ^ Write data
      , _wlast :: IsLast
      -- ^ Write last
      , _wuser :: userType
      -- ^ User data
      }
  deriving (Generic)

-- | See Table A2-3 "Write data channel signals"
newtype S2M_WriteData = S2M_WriteData {_wready :: Bool}
  deriving stock (Show, Generic)
  deriving anyclass (C.NFDataX, C.BitPack)

{- | Shorthand for a "well-behaved" write data config,
so that we don't need to write out a bunch of type constraints later.
Holds for every configuration; don't worry about implementing this class.
-}
type KnownAxi4WriteDataConfig conf =
  ( KeepStrobeClass (WKeepStrobe conf)
  , C.KnownNat (WNBytes conf)
  , Eq (StrobeDataType (WKeepStrobe conf))
  , Show (StrobeDataType (WKeepStrobe conf))
  , C.ShowX (StrobeDataType (WKeepStrobe conf))
  , NFData (StrobeDataType (WKeepStrobe conf))
  , C.NFDataX (StrobeDataType (WKeepStrobe conf))
  , C.BitPack (StrobeDataType (WKeepStrobe conf))
  )

deriving instance
  ( KnownAxi4WriteDataConfig conf
  , Show userType
  ) =>
  Show (M2S_WriteData conf userType)

deriving instance
  ( KnownAxi4WriteDataConfig conf
  , C.ShowX userType
  ) =>
  C.ShowX (M2S_WriteData conf userType)

deriving instance
  ( KnownAxi4WriteDataConfig conf
  , Eq userType
  ) =>
  Eq (M2S_WriteData conf userType)

deriving instance
  ( KnownAxi4WriteDataConfig conf
  , C.BitPack userType
  ) =>
  C.BitPack (M2S_WriteData conf userType)

deriving instance
  ( KnownAxi4WriteDataConfig conf
  , C.NFDataX userType
  ) =>
  C.NFDataX (M2S_WriteData conf userType)

{- | Mainly for use in @DfConv@.

Data carried along 'Axi4WriteData' channel which is put in control of
the user, rather than being managed by the @DfConv@ instances. Matches up
one-to-one with the fields of 'M2S_WriteData' except for '_wlast'.
-}
data Axi4WriteDataInfo (conf :: Axi4WriteDataConfig) (userType :: Type) = Axi4WriteDataInfo
  { _widata :: StrictStrobeType (WNBytes conf) (WKeepStrobe conf)
  -- ^ Write data
  , _wiuser :: userType
  -- ^ User data
  }
  deriving (Generic)

deriving instance
  ( KnownAxi4WriteDataConfig conf
  , Show userType
  ) =>
  Show (Axi4WriteDataInfo conf userType)

deriving instance
  ( KnownAxi4WriteDataConfig conf
  , C.ShowX userType
  ) =>
  C.ShowX (Axi4WriteDataInfo conf userType)

deriving instance
  ( KnownAxi4WriteDataConfig conf
  , Eq userType
  ) =>
  Eq (Axi4WriteDataInfo conf userType)

deriving instance
  ( KnownAxi4WriteDataConfig conf
  , C.NFDataX userType
  ) =>
  C.NFDataX (Axi4WriteDataInfo conf userType)

deriving instance
  ( KnownAxi4WriteDataConfig conf
  , NFData userType
  ) =>
  NFData (Axi4WriteDataInfo conf userType)

-- | Convert 'M2S_WriteData' to 'Axi4WriteDataInfo', dropping some info
axi4WriteDataMsgToWriteDataInfo ::
  M2S_WriteData conf userType ->
  Axi4WriteDataInfo conf userType
axi4WriteDataMsgToWriteDataInfo M2S_NoWriteData = C.errorX "Expected WriteData"
axi4WriteDataMsgToWriteDataInfo M2S_WriteData{..} =
  Axi4WriteDataInfo
    { _widata = _wdata
    , _wiuser = _wuser
    }

-- | Convert 'Axi4WriteDataInfo' to 'M2S_WriteData', adding some info
axi4WriteDataMsgFromWriteDataInfo ::
  IsLast ->
  Axi4WriteDataInfo conf userType ->
  M2S_WriteData conf userType
axi4WriteDataMsgFromWriteDataInfo _wlast Axi4WriteDataInfo{..} =
  M2S_WriteData
    { _wdata = _widata
    , _wuser = _wiuser
    , _wlast
    }

instance
  (KnownAxi4WriteDataConfig conf, C.NFDataX userType) =>
  DfConv.DfConv (Axi4WriteData dom conf userType)
  where
  type Dom (Axi4WriteData dom conf userType) = dom
  type
    FwdPayload (Axi4WriteData dom conf userType) =
      ( Axi4WriteDataInfo conf userType
      , IsLast
      )

  toDfCircuit Proxy = Circuit $ B.first unbundle . unbundle . fmap go . bundle . B.first bundle
   where
    go ~(~(dfFwdM2S, _dfBwdS2M), wdS2M) = ((dfFwdS2M, dfBwdM2S), wdM2S)
     where
      wdM2S = case dfFwdM2S of
        Just (info, isLast) -> axi4WriteDataMsgFromWriteDataInfo isLast info
        Nothing -> M2S_NoWriteData
      dfFwdS2M = Ack wdS2M._wready
      dfBwdM2S = Nothing

  fromDfCircuit Proxy = Circuit $ B.second unbundle . unbundle . fmap go . bundle . B.second bundle
   where
    go ~(wdM2S, ~(Ack dfFwdS2M, _dfBwdM2S)) = (wdS2M, (dfFwdM2S, dfBwdS2M))
     where
      wdS2M = S2M_WriteData dfFwdS2M
      dfFwdM2S = case wdM2S of
        m2s@M2S_WriteData{} ->
          Just
            ( axi4WriteDataMsgToWriteDataInfo m2s
            , m2s._wlast
            )
        M2S_NoWriteData{} -> Nothing
      dfBwdS2M = Ack False

instance
  (KnownAxi4WriteDataConfig conf, C.NFDataX userType, C.KnownDomain dom) =>
  Simulate (Axi4WriteData dom conf userType)
  where
  type
    SimulateFwdType (Axi4WriteData dom conf userType) =
      [M2S_WriteData conf userType]
  type SimulateBwdType (Axi4WriteData dom conf userType) = [S2M_WriteData]
  type SimulateChannels (Axi4WriteData dom conf userType) = 1

  simToSigFwd _ = C.fromList_lazy
  simToSigBwd _ = C.fromList_lazy
  sigToSimFwd _ s = C.sample_lazy s
  sigToSimBwd _ s = C.sample_lazy s

  stallC conf (C.head -> (stallAck, stalls)) =
    C.withClockResetEnable C.clockGen C.resetGen C.enableGen $
      DfConv.stall Proxy Proxy conf stallAck stalls

instance
  (KnownAxi4WriteDataConfig conf, C.NFDataX userType, C.KnownDomain dom) =>
  Drivable (Axi4WriteData dom conf userType)
  where
  type
    ExpectType (Axi4WriteData dom conf userType) =
      [M2S_WriteData conf userType]

  toSimulateType Proxy = id
  fromSimulateType Proxy = filter p
   where
    -- TODO: Should we filter out M2S_NoWriteAddress like we do here?
    p M2S_WriteData{} = True
    p M2S_NoWriteData = False

  driveC conf vals =
    C.withClockResetEnable C.clockGen C.resetGen C.enableGen $
      DfConv.drive Proxy conf (map toInfo vals)
   where
    toInfo m2s@M2S_WriteData{} =
      Just (axi4WriteDataMsgToWriteDataInfo m2s, m2s._wlast)
    toInfo M2S_NoWriteData = Nothing
  sampleC conf ckt =
    map fromInfo $
      C.withClockResetEnable C.clockGen C.resetGen C.enableGen $
        DfConv.sample Proxy conf ckt
   where
    fromInfo (Just (i, l)) = axi4WriteDataMsgFromWriteDataInfo l i
    fromInfo Nothing = M2S_NoWriteData

instance
  ( KnownAxi4WriteDataConfig conf
  , NFData (M2S_WriteData conf userType)
  , C.NFDataX userType
  , NFData userType
  , C.ShowX userType
  , Show userType
  , Eq userType
  , C.KnownDomain dom
  ) =>
  Test (Axi4WriteData dom conf userType)
  where
  expectN Proxy eOpts simFwd =
    fmap (map fromInfo) $ expectN (Proxy @(Df.Df dom _)) eOpts $ map toInfo simFwd
   where
    toInfo m2s@M2S_WriteData{} =
      Just (axi4WriteDataMsgToWriteDataInfo m2s, m2s._wlast)
    toInfo M2S_NoWriteData = Nothing

    fromInfo (i, l) = axi4WriteDataMsgFromWriteDataInfo l i

instance IdleCircuit (Axi4WriteData dom conf userType) where
  idleFwd _ = C.pure M2S_NoWriteData
  idleBwd _ = C.pure S2M_WriteData{_wready = False}

{- | Force a /nack/ on the backward channel and /no data/ on the forward
channel if reset is asserted.
-}
forceResetSanity ::
  (C.KnownDomain dom, C.HiddenReset dom) =>
  Circuit (Axi4WriteData dom conf userType) (Axi4WriteData dom conf userType)
forceResetSanity = forceResetSanityGeneric
