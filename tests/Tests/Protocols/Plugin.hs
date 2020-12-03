

{-# LANGUAGE Arrows              #-}
{-# LANGUAGE BlockArguments      #-}
{-# LANGUAGE DeriveFunctor       #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE DataKinds           #-}

{-# OPTIONS -fplugin=Protocols.Plugin #-}

module Tests.Protocols.Plugin where

import qualified Clash.Prelude as C

import           Protocols
-- import           Protocols.Df.Simple (Dfs)
-- import qualified Protocols.Df.Simple as Dfs

swapC :: Circuit (a, b) (b, a)
swapC = circuit $ \(a, b) -> (b, a)

unvecC :: Circuit (C.Vec 2 a) (a, a)
unvecC = circuit \[x,y] -> (x, y)

-- registerBoth :: Circuit (Dfs dom a, Dfs dom b) (Dfs dom a, Dfs dom b)
-- registerBoth = circuit $ \(a, b) -> do
--   a' <- Dfs.registerFwd -< a
--   b' <- Dfs.registerFwd -< b
--   idC -< (a', b')
