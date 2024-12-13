module Data.Maybe.Extra (
  toMaybe,
) where

-- | Wrap a value in a @Just@ if @True@
toMaybe :: Bool -> a -> Maybe a
toMaybe True x = Just x
toMaybe False _ = Nothing
