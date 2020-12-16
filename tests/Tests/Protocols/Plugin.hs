
-- The plugin relies on Arrows and BlockArguments to be enabled. These
-- dependencies will be dropped in the future.
{-# LANGUAGE Arrows #-}
{-# LANGUAGE BlockArguments #-}

-- | This /must/ be enabled in order for the plugin to do its work. You might
-- want to add this to 'ghc-options' in your cabal file.
{-# OPTIONS -fplugin=Protocols.Plugin #-}

module Tests.Protocols.Plugin where

import qualified Clash.Prelude as C

import           Protocols
import qualified Protocols.Df.Simple as Dfs

-- | Simply swap two streams. Note that the 'circuit' is a magic keyword the
-- 'Protocols.Plugin' looks for in order to do its work.
swapC :: Circuit (a, b) (b, a)
swapC = circuit $ \(a, b) -> (b, a)


-- | Put 'registerFwd' on both 'Dfs' input streams.
registerBoth ::
  (C.NFDataX a, C.NFDataX b, C.HiddenClockResetEnable dom) =>
  Circuit (Dfs dom a, Dfs dom b) (Dfs dom a, Dfs dom b)
registerBoth = circuit $ \(a, b) -> do
  -- We route /a/ to into a 'registerFwd'. Note that this takes care of routing
  -- both the /forward/ and /backward/ parts, even though it seems that it only
  -- handles the /forward/ part.
  a' <- Dfs.registerFwd -< a

  -- Similarly, we route /b/ to a register too
  b' <- Dfs.registerFwd -< b

  -- The final line of a circuit-do block needs to be an "assignment". Because
  -- we want to simply bundle two streams, we use 'idC' as our circuit of choice.
  idC -< (a', b')


-- | Fanout a stream and interact with some of the result streams.
fanOutThenRegisterMiddle ::
  C.HiddenClockResetEnable dom =>
  Circuit (Dfs dom Int) (Dfs dom Int, Dfs dom Int, Dfs dom Int)
fanOutThenRegisterMiddle = circuit $ \a -> do
  -- List notation can be used to specify a Vec. In this instance, fanout will
  -- infer that it needs to produce a 'Vec 3 Int'.
  [x, y, z] <- Dfs.fanout -< a

  -- Like in 'registerBoth', we can put a register on the forward part of 'y'.
  y' <- Dfs.registerFwd -< y

  -- We can use any Haskell notation between the arrows, as long as it results
  -- in a properly typed circuit. For example, we could map the function (+5)
  -- over the stream 'z'.
  z' <- Dfs.map (+5) -< z

  idC -< (x, y', z')


-- | Forget the /left/ part of a tuple of 'Dfs' streams
forgetLeft :: Circuit (Dfs dom a, Dfs dom b) (Dfs dom b)
forgetLeft = circuit $ \(a, b) -> do
  -- We can use an underscore to indicate that we'd like to throw away any
  -- data from stream 'a'. For 'Dfs' like protocols, a constant acknowledgement
  -- will be driven on the /backwards/ part of the protocol.
  _a <- idC -< a

  idC -< b


-- | Forget the /left/ part of a tuple of 'Dfs' streams.
forgetLeft2 :: Circuit (Dfs dom a, Dfs dom b) (Dfs dom b)
forgetLeft2 =
  -- If we know right from the start that'd we'd like to ignore an incoming
  -- stream, we can simply mark it with an underscore.
  circuit $ \(_a, b) -> b


-- | Convert a 2-vector into a 2-tuple
unvec :: Circuit (C.Vec 2 a) (a, a)
unvec =
  -- We don't always need /do/ notation
  circuit \[x,y] -> (x, y)
