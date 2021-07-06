module Platinum.Contracts.Utils where

{-# INLINABLE guard #-}
guard :: Bool -> Maybe ()
guard False = Nothing
guard True = Just ()