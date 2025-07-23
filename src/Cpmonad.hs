{-# LANGUAGE NoFieldSelectors #-}

module Cpmonad(
  Verdict(..),
  mergeVerdict',
  Problem(..),

  Gen(..),
  runGen,
  genr,
  genri,
  Indexable(..),
  choose,
) where

import Data.List
import Data.Vector qualified as V
import Data.Vector.Strict qualified
import System.Random (uniformR, StdGen)
import Printer

-- [[[ Basics ]]]
-- AC x  where x must be between 0 and 1

data Verdict = AC Float | WA | RE | TLE | Other

mergeVerdict' :: (Verdict, b) -> (Verdict, b) -> (Verdict, b)
mergeVerdict' a b = case (a, b) of
  ((AC x, _), (AC y, _))
    | x < y -> a
    | otherwise -> b
  ((AC _, _), _) -> b
  (_, (AC _, _)) -> a
  (_, _) -> a

instance Semigroup Verdict where
  a <> b = fst $ mergeVerdict' (a, ()) (b, ())

instance Monoid Verdict where
  mempty = AC 1

data Problem i o a = Problem
  { tests :: [(i, a)],
    sols :: [i -> o],
    check :: i -> a -> o -> Bool,
    printerI :: Printer i,
    printerO :: Printer o,
    printerA :: Printer a,
    ei :: i, eo :: o, ea :: a
  }

-- [[[ Data generation ]]]

newtype Gen a = Gen { runGen :: StdGen -> (a, StdGen) } deriving (Functor)

runGen :: Gen a -> StdGen -> (a, StdGen)
runGen (Gen a) = a

instance Applicative Gen where
  pure x = Gen (x,)
  Gen h1 <*> Gen h2 = Gen $ \g ->
    let (a1, g1) = h1 g
        (a2, g2) = h2 g1
     in (a1 a2, g2)

instance Monad Gen where
  Gen h1 >>= next = Gen $ \g ->
    let (a1, g1) = h1 g
        (a2, g2) = runGen (next a1) g1
     in (a2, g2)

genr :: Int -> Int -> Gen Int
genr a b = Gen $ uniformR (a, b-1)

genri :: Int -> Int -> Gen Int
genri a b = genr a (b-1)

class Indexable c where
  index :: c a -> Int -> a
  size :: c a -> Int

instance Indexable [] where
  index = (Data.List.!!)
  size = Data.List.length

instance Indexable V.Vector where
  index = (V.!)
  size = V.length

instance Indexable Data.Vector.Strict.Vector where
  index = (Data.Vector.Strict.!)
  size = Data.Vector.Strict.length

choose :: Indexable c => c a -> Gen a
choose xs = let n = size xs in index xs <$> genr 0 n
