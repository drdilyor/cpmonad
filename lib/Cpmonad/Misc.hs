{-# OPTIONS_GHC -Wno-orphans #-}

-- | Miscellaneous useful functions and orphan instances.
module Cpmonad.Misc where

import Data.Default
import Data.Vector (Vector)
import Data.Vector qualified as V
import Data.Vector.Strict qualified as VS
import Data.Vector.Algorithms.Merge qualified as VA

-- Utility functions

-- | Sort a vector
vsort :: Vector Int -> Vector Int
vsort v' = V.create do
  v <- V.unsafeThaw v'
  VA.sort v
  pure v

-- | Inverse of a permutation
vinverse :: Vector Int -> Vector Int
vinverse p = V.update_ p p (V.enumFromN 0 $ V.length p)

-- Orphan instances

instance Default (V.Vector a) where
  def = V.empty

instance Default (VS.Vector a) where
  def = VS.empty
