module Platinum.Contracts.Utils where

import PlutusTx.AssocMap (Map, lookup)
import PlutusTx.Prelude

{-# INLINABLE guard #-}
guard :: Bool -> Maybe ()
guard False = Nothing
guard True = Just ()

{-# INLINABLE lookupDefault #-}
lookupDefault :: Eq k => v -> k -> Map k v -> v
lookupDefault def k = fromMaybe def . lookup k