{-# LANGUAGE OverloadedStrings #-}

module Protocols.Test.Samples.Vcd where

import Data.Bits (Bits (testBit))
import "extra" Data.List.Extra (dropEnd)
import Data.List.Infinite (Infinite ((:<)), (...))
import Data.List.NonEmpty (NonEmpty ((:|)), nonEmpty)
import Data.Map (Map)
import Data.Maybe (catMaybes)
import Data.Ord (comparing)
import Data.Text (Text)
import Data.Tuple.Extra (fst3)
import Numeric.Natural (Natural)
import Protocols.Test.Samples (
  Sample (Sample, mask, value),
  StatusSample,
  statusSampleToSample,
 )

import Data.Char qualified as Char
import Data.List qualified as L
import Data.List.Infinite qualified as Infinite
import Data.List.NonEmpty qualified as NonEmpty
import Data.Map qualified as Map
import Data.String.Interpolate qualified as I
import Data.Text qualified as Text

type NonEmptyString = NonEmpty Char
type HierarchicalName = NonEmpty NonEmptyString

{- $setup
>>> import Data.List qualified as L
>>> import Data.List.NonEmpty qualified as NonEmpty
>>> pretty = hierarchicalNameToList
-}

{- | Convert a trace name with possible empty segments into a a non-empty
hierarchical name.

== Examples
| >>> pretty $ toHierarchicalName ["foo", "bar", "baz"]
["foo","bar","baz"]
| >>> pretty $ toHierarchicalName ["foo", "", "baz"]
["foo","baz"]
| >>> pretty $ toHierarchicalName ["foo", "", ""]
["foo","NO_NAME_TRACE"]
| >>> pretty $ toHierarchicalName ["foo", "bar", ""]
["foo","bar","NO_NAME_TRACE"]
| >>> pretty $ toHierarchicalName [""]
["NO_NAME_TRACE"]
| >>> pretty $ toHierarchicalName []
["NO_NAME_TRACE"]
-}
toHierarchicalName :: [String] -> HierarchicalName
toHierarchicalName = go . nonEmpty . fmap nonEmpty
 where
  go :: Maybe (NonEmpty (Maybe (NonEmpty Char))) -> HierarchicalName
  go Nothing = noNameTrace :| []
  go (Just names)
    | Nothing <- NonEmpty.last names =
        go $
          Just $
            NonEmpty.prependList
              (NonEmpty.init names)
              (pure (Just noNameTrace))
    | otherwise =
        case catMaybes (NonEmpty.toList names) of
          [] -> go Nothing
          (x : xs) -> x :| xs

  noNameTrace :: NonEmptyString
  noNameTrace = 'N' :| "O_NAME_TRACE"


hierarchicalNameToList :: HierarchicalName -> [Text]
hierarchicalNameToList = fmap Text.pack . NonEmpty.toList . fmap NonEmpty.toList

{- | Infinite stream of identifiers, consisting of printable ASCII characters. When
the single character identifiers are exhausted, it will start generating
identifiers with two characters, then three, and so on.
-}
identifiers :: Infinite Text
identifiers = Text.pack <$> Infinite.concatMap (NonEmpty.fromList . go) (1 ...)
 where
  go :: Word -> [String]
  go 0 = [""]
  go n = [c : s | c <- chars, s <- go (n - 1)]

  chars :: [Char]
  chars = [c | i <- [0 .. 127], let c = Char.chr i, Char.isPrint c]

data VcdHeader = VcdHeader
  { date :: Text
  , version :: Text
  , timescale :: Text
  , scopes :: [Scope]
  }

data Scope
  = Scope {name :: Text}
  | Wire {name :: Text, label :: Text, width :: Int, init :: Text}
  | Reg {name :: Text, label :: Text, width :: Int, init :: Text}
  | StopScope
  deriving (Eq, Show)

renderScopes :: [Scope] -> Text
renderScopes = Text.intercalate "\n" . snd . L.mapAccumL go (0 :: Int)
 where
  indentBy = 2

  go indent = \case
    Scope name ->
      ( indent + indentBy
      , [I.i|#{indentation}$scope module #{name} $end|]
      )
    Wire name label width _init ->
      ( indent
      , [I.i|#{indentation}$var wire #{width} #{label} #{name} $end|]
      )
    Reg name label width _init ->
      ( indent
      , [I.i|#{indentation}$var reg #{width} #{label} #{name} $end|]
      )
    StopScope ->
      ( indent - indentBy
      , "$upscope $end"
      )
   where
    indentation = Text.replicate indent " "

renderInits :: [Scope] -> Text
renderInits =
  ("$dumpvars\n" <>)
    . (<> "\n$end")
    . Text.intercalate "\n"
    . catMaybes
    . fmap go
 where
  go :: Scope -> Maybe Text
  go = \case
    Wire _name label _width initVal -> Just (initVal <> label)
    Reg _name label _width initVal -> Just (initVal <> label)
    Scope _name -> Nothing
    StopScope -> Nothing

renderHeader :: VcdHeader -> Text
renderHeader VcdHeader{date, version, timescale, scopes} =
  [I.i|$date #{date} $end
$version #{version} $end
$timescale #{timescale} $end

// Scopes
#{renderScopes scopes}
$enddefinitions $end

// Initial values
$dumpvars
#{renderInits scopes}
$end

// Value changes
\#0
|]

renderSample :: Int -> Text -> Sample -> Text
renderSample 1 label sample = case sample of
  Sample{mask = 0, value = 0} -> "0" <> label
  Sample{mask = 0} -> "1" <> label
  Sample{} -> "x" <> label
renderSample width label sample =
  Text.pack ('b' : rendered) <> label
 where
  rendered :: [Char]
  rendered = case sample of
    Sample{mask = 0, value = 0} -> L.replicate width '0'
    Sample{mask = 0, value} -> renderDefined <$> toBits value
    Sample{mask, value} -> renderUndefined <$> toBits mask <*> toBits value

  toBits :: Natural -> [Bool]
  toBits v = testBit v <$> [width - 1, width - 2 .. 0]

  renderDefined :: Bool -> Char
  renderDefined True = '1'
  renderDefined False = '0'

  renderUndefined :: Bool -> Bool -> Char
  renderUndefined False False = '0'
  renderUndefined False True = '1'
  renderUndefined True _ = 'x'

renderStatusSample :: Text -> StatusSample -> Text
renderStatusSample label = renderSample 2 label . statusSampleToSample

type VcdId = Text
type Width = Int

toScopes ::
  [(HierarchicalName, Width, VcdId)] ->
  ( [Scope]
  , Map HierarchicalName VcdId
  )
toScopes names = (go identifiers [] sortedNames, idMap)
 where
  sortedNames :: [(HierarchicalName, Width, VcdId)]
  sortedNames = L.sortBy (comparing (\l -> (length (fst3 l), NonEmpty.init (fst3 l)))) names

  idMap =
    Map.fromList $
      L.zip
        (fst3 <$> sortedNames)
        (Infinite.toList identifiers)

  go :: Infinite Text -> [Text] -> [(HierarchicalName, Int, Text)] -> [Scope]
  go _ prefix [] = L.replicate (L.length prefix) StopScope
  go (i :< is) prefix ((t, width, initVal) : ts)
    -- Fooooooo
    | prefix `L.isPrefixOf` tt
    , prefix == dropEnd 1 tt =
        Wire (name tt) i width initVal : go is prefix ts
    -- Baaaar
    | prefix `L.isPrefixOf` tt =
        let scope = nextScope prefix tt
         in Scope scope : go (i :< is) (prefix <> [scope]) ((t, width, initVal) : ts)
    -- Baaaar
    | otherwise = StopScope : go (i :< is) (L.init prefix) ((t, width, initVal) : ts)
   where
    tt = hierarchicalNameToList t

  name :: [t] -> t
  name t = case L.drop (L.length t - 1) t of
    [] -> error "toScopes: unreachable"
    (n : _) -> n

  nextScope :: [Text] -> [Text] -> Text
  nextScope prefix ts = case L.drop (L.length prefix) ts of
    [] -> error "toScopes: unreachable"
    (n : _) -> n
