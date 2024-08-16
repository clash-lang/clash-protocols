{-# LANGUAGE BlockArguments #-}

{-# OPTIONS -fplugin=Protocols.Plugin #-}

-- For debugging purposes:
-- {-# OPTIONS -fplugin-opt=Protocols.Plugin:debug #-}

{- | This /must/ be enabled in order for the plugin to do its work. You might
want to add this to 'ghc-options' in your cabal file.
-}
module Tests.Protocols.Plugin where

import qualified Clash.Prelude as C

import Protocols
import qualified Protocols.Df as Df

{- | Simply swap two streams. Note that the 'circuit' is a magic keyword the
'Protocols.Plugin' looks for in order to do its work.
-}
swapC :: Circuit (a, b) (b, a)
swapC = circuit $ \(a, b) -> (b, a)

-- | Put 'registerFwd' on both 'Df' input streams.
registerBoth ::
  (C.NFDataX a, C.NFDataX b, C.HiddenClockResetEnable dom) =>
  Circuit (Df dom a, Df dom b) (Df dom a, Df dom b)
registerBoth = circuit $ \(a, b) -> do
  -- We route /a/ to into a 'registerFwd'. Note that this takes care of routing
  -- both the /forward/ and /backward/ parts, even though it seems that it only
  -- handles the /forward/ part.
  a' <- Df.registerFwd -< a

  -- Similarly, we route /b/ to a register too
  b' <- Df.registerFwd -< b

  -- The final line of a circuit-do block needs to be an "assignment". Because
  -- we want to simply bundle two streams, we use 'idC' as our circuit of choice.
  idC -< (a', b')

-- | Fanout a stream and interact with some of the result streams.
fanOutThenRegisterMiddle ::
  (C.HiddenClockResetEnable dom) =>
  Circuit (Df dom Int) (Df dom Int, Df dom Int, Df dom Int)
fanOutThenRegisterMiddle = circuit $ \a -> do
  -- List notation can be used to specify a Vec. In this instance, fanout will
  -- infer that it needs to produce a 'Vec 3 Int'.
  [x, y, z] <- Df.fanout -< a

  -- Like in 'registerBoth', we can put a register on the forward part of 'y'.
  y' <- Df.registerFwd -< y

  -- We can use any Haskell notation between the arrows, as long as it results
  -- in a properly typed circuit. For example, we could map the function (+5)
  -- over the stream 'z'.
  z' <- Df.map (+ 5) -< z

  idC -< (x, y', z')

-- | Forget the /left/ part of a tuple of 'Df' streams
forgetLeft :: Circuit (Df dom a, Df dom b) (Df dom b)
forgetLeft = circuit $ \(a, b) -> do
  -- We can use an underscore to indicate that we'd like to throw away any
  -- data from stream 'a'. For 'Df' like protocols, a constant acknowledgement
  -- will be driven on the /backwards/ part of the protocol.
  _a <- idC -< a

  idC -< b

-- | Forget the /left/ part of a tuple of 'Df' streams.
forgetLeft2 :: Circuit (Df dom a, Df dom b) (Df dom b)
forgetLeft2 =
  -- If we know right from the start that'd we'd like to ignore an incoming
  -- stream, we can simply mark it with an underscore.
  circuit $ \(_a, b) -> b

-- | Convert a 2-vector into a 2-tuple
unvec :: Circuit (C.Vec 2 a) (a, a)
unvec =
  -- We don't always need /do/ notation
  circuit \[x, y] -> (x, y)
