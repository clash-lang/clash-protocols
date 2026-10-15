{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoFieldSelectors #-}

{- |
Rendering of Value Change Dump (VCD) files, as specified in IEEE 1364. These can
be opened with waveform viewers such as GTKWave or Surfer. Used by
'Protocols.Experimental.Testbench.runTestbench'.
-}
module Protocols.Experimental.Testbench.Vcd (
  VcdVar (..),
  identifiers,
  renderHeader,
  renderSample,
) where

import Data.Bits (testBit)
import Data.List qualified as L
import Data.Text (Text)
import Data.Text qualified as Text
import Numeric.Natural (Natural)
import Protocols.Experimental.Testbench.Trace (Sample (..))

{- $setup
>>> :set -XOverloadedStrings
>>> import Protocols.Experimental.Testbench.Trace (Sample (..))
-}

-- | A variable in a VCD file
data VcdVar = VcdVar
  { scopes :: ![Text]
  -- ^ Scopes the variable lives in, outermost first
  , name :: !Text
  -- ^ Name of the variable
  , identifier :: !Text
  -- ^ Short identifier used in value changes, see 'identifiers'
  , width :: !Int
  -- ^ Width of the variable in bits
  }
  deriving (Eq, Show)

{- | Infinite list of unique VCD identifiers, made of printable ASCII characters
except whitespace. Once all single character identifiers are used, it continues
with two characters, and so on.

>>> take 3 identifiers
["!","\"","#"]
>>> identifiers !! 94
"!!"
-}
identifiers :: [Text]
identifiers = Text.pack <$> L.concatMap go [1 ..]
 where
  go :: Int -> [String]
  go 0 = [""]
  go n = [c : s | c <- ['!' .. '~'], s <- go (n - 1)]

-- | Tree of scopes, preserving the order in which variables were added
newtype ScopeTree = ScopeTree [(Text, Either VcdVar ScopeTree)]

insertVar :: VcdVar -> ScopeTree -> ScopeTree
insertVar var = go var.scopes
 where
  go [] (ScopeTree entries) = ScopeTree (entries <> [(var.name, Left var)])
  go (s : ss) (ScopeTree entries) =
    case L.break isScope entries of
      (before, (_, Right sub) : after) -> ScopeTree (before <> [(s, Right (go ss sub))] <> after)
      _ -> ScopeTree (entries <> [(s, Right (go ss (ScopeTree [])))])
   where
    isScope (n, Right _) = n == s
    isScope _ = False

renderScopeTree :: Int -> ScopeTree -> [Text]
renderScopeTree depth (ScopeTree entries) = L.concatMap goEntry entries
 where
  indent = Text.replicate depth "  "
  goEntry (_, Left var) =
    [ indent
        <> "$var wire "
        <> Text.pack (show var.width)
        <> " "
        <> var.identifier
        <> " "
        <> sanitize var.name
        <> " $end"
    ]
  goEntry (name, Right sub) =
    [indent <> "$scope module " <> sanitize name <> " $end"]
      <> renderScopeTree (depth + 1) sub
      <> [indent <> "$upscope $end"]

-- | VCD names cannot contain whitespace
sanitize :: Text -> Text
sanitize = Text.map (\c -> if c == ' ' || c == '\t' || c == '\n' then '_' else c)

{- | Render the header of a VCD file, declaring all variables. Variables are
placed in a top-level scope called @testbench@. Value changes, including the
initial values, should follow.
-}
renderHeader :: [VcdVar] -> Text
renderHeader vars =
  Text.unlines $
    [ "$version clash-protocols testbench $end"
    , "$timescale 1fs $end"
    , "$scope module testbench $end"
    ]
      <> renderScopeTree 1 (L.foldl' (flip insertVar) (ScopeTree []) vars)
      <> [ "$upscope $end"
         , "$enddefinitions $end"
         ]

{- | Render a single value change, including the trailing newline.

>>> renderSample 1 "!" (Sample 0 1)
"1!\n"
>>> renderSample 4 "#" (Sample 0b0100 0b0001)
"b0x01 #\n"
-}
renderSample :: Int -> Text -> Sample -> Text
renderSample 1 ident sample = Text.cons (bit sample.mask sample.value 0) ident <> "\n"
renderSample width ident sample =
  Text.pack ('b' : [bit sample.mask sample.value i | i <- [width - 1, width - 2 .. 0]])
    <> " "
    <> ident
    <> "\n"

bit :: Natural -> Natural -> Int -> Char
bit msk val i
  | testBit msk i = 'x'
  | testBit val i = '1'
  | otherwise = '0'
