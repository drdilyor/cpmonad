{-# OPTIONS_GHC -Wno-orphans #-}

module Cpmonad.TestUtil where

import Test.QuickCheck
import System.Random

instance Arbitrary StdGen where
  arbitrary = mkStdGen <$> chooseInt (minBound, maxBound)
  shrink _ = []
