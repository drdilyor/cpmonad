{-# OPTIONS_GHC -Wno-missing-signatures #-}

module Cpmonad.GenSpec where

import Data.List (sort)
import Data.Vector qualified as V
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck hiding (Gen)
import Prelude hiding (print)

import Control.Monad (replicateM)
import Cpmonad.Gen hiding (choose, shuffle)
import Cpmonad.Gen qualified
import Cpmonad.TestUtil ()
import Data.Containers.ListUtils (nubOrd)
import System.Random (mkStdGen)

spec :: Spec
spec = do
  describe "primitives" do
    prop "genr is within range" $
      \(runGen (genr 1 5) -> (x, _)) ->
        collect x $
          1 <= x && x < 5
    prop "genri is within range" $
      \(runGen (genr 1 5) -> (x, _)) ->
        collect x $
          1 <= x && x <= 5
    describe "genrw" $ do
      prop "is within range" $
        \(runGen (genrw 1 5 2) -> (x, _)) ->
          collect x $
            1 <= x && x < 5
      -- TODO: i am giving up qc's builtin statistical fluke check
      let n = 100
          x = 100
          x' = 90
          w = 20
      prop "is skewed" $
        \(runGen (replicateM n $ genrw 0 x w) -> (xs, _)) ->
          let cnt = length (filter (>= x') xs)
           in label ("count of (>=" <> show x' <> "%) = " <> show cnt) $
                cnt >= n `div` 2

    let xs = [1, 2, 6, 7 :: Int]
     in prop "choose" $
          \(runGen (Cpmonad.Gen.choose xs) -> (x, _)) ->
            x `elem` xs

    prop "genuntil" $
      \(runGen (genuntil (<= 2) (genri 1 3)) -> (x, _)) ->
        1 <= x && x <= 2

    describe "genpair" do
      prop "(<)" $ \(runGen (genpair (<) (genri 1 4)) -> ((a, b), _)) -> a < b
      prop "(<=)" $ \(runGen (genpair (<=) (genri 1 4)) -> ((a, b), _)) -> a <= b
      prop "(==)" $ \(runGen (genpair (==) (genri 1 4)) -> ((a, b), _)) -> a == b

  describe "distribute" do
    prop "m = 0" $
      \(getNonNegative -> s)
       (getPositive -> n)
       (runGen (distribute s n 0) -> (x, _)) ->
          counterexample ("x = " <> show x) $
            (V.length x === n) .&&. (V.sum x === s) .&&. V.minimum x >= 0

    prop "m > 0" $
      \(getNonNegative -> s')
       (getPositive -> n)
       (getPositive -> m)
       (runGen (distribute (s' + n * m) n m) -> (x, _)) ->
          counterexample ("x = " <> show x) $
            V.length x == n && V.sum x == (s' + n * m) && V.minimum x >= m

  let covers :: (Eq a, Ord a) => Int -> Gen a -> Expectation
      covers n gen = length (nubOrd . fst . runGen (replicateM nsafe gen) $ mkStdGen 1) `shouldBe` n
       where
        nsafe = (floor :: Double -> Int) $ fromIntegral n * (log (fromIntegral n) + 15.0)

  describe "shuffle" do
    prop "shuffles" $
      \(xs :: [Int]) (runGen (Cpmonad.Gen.shuffle $ V.fromList xs) -> (V.toList -> ys, _)) ->
        counterexample (show ys) $
          sort xs === sort ys
    let n = 4
        xs = V.enumFromN (0 :: Int) n
        nfac = product [1 .. n]
    it "covers" $ covers nfac $ Cpmonad.Gen.shuffle xs

  describe "genperm" do
    prop "generates a permutation" $
      \n (runGen (genperm n) -> (V.toList -> xs, _)) ->
        counterexample (show xs) $ sort xs == [0 .. n - 1]

  describe "gendistinct" do
    prop "values are distinct" $
      \(getNonNegative -> n)
       (runGen (gendistinct n $ genr 0 (n * 2)) -> (V.toList -> xs, _)) ->
          counterexample (show xs) $
            length (nubOrd xs) == n
    it "covers" $ covers 2520 $ gendistinct 5 (genr 0 7)
