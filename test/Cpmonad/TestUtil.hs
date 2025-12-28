{-# OPTIONS_GHC -Wno-orphans #-}

module Cpmonad.TestUtil where

import Control.Monad
import Cpmonad (Gen, runGen)
import Data.Containers.ListUtils (nubOrd)
import System.Random
import Test.Hspec (Expectation, shouldBe)
import Test.QuickCheck

instance Arbitrary StdGen where
  arbitrary = mkStdGen <$> chooseInt (minBound, maxBound)
  shrink _ = []

covers :: (Ord a) => Int -> Cpmonad.Gen a -> Expectation
covers n gen = length (nubOrd . fst . runGen (replicateM nsafe gen) $ mkStdGen 1) `shouldBe` n
 where
  nsafe = (floor :: Double -> Int) $ fromIntegral n * (log (fromIntegral n) + 15.0)
