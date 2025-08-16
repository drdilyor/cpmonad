module Cpmonad.Gen(
  Gen,
  GenST,
  runGen,
  runGenST,
  liftg,
  runIOGen,

  genr,
  genri,
  Indexable(..),
  choose,
  genpair,
  distribute,
  shuffle,
  genperm,
  gendistinct,
  lift,
  state,
) where

import Control.Monad.ST
import Control.Monad.State.Strict (State, StateT, state, lift, runState, runStateT)
import Data.List
import Data.Set qualified as Set
import Data.Vector (Vector, (!))
import Data.Vector qualified as V
import Data.Vector.Mutable qualified as VM
import Data.Vector.Strict qualified as VS
import Data.Vector.Algorithms.Merge qualified as VA
import System.Random (uniformR, StdGen, newStdGen)

type Gen = State StdGen
type GenST s = StateT StdGen (ST s)

runGen :: Gen a -> StdGen -> (a, StdGen)
runGen = runState

runGenST :: (forall s. GenST s a) -> Gen a
runGenST g = state \s -> runST $ runStateT g s

liftg :: Gen a -> GenST s a
liftg = state . runState

runIOGen :: Gen a -> IO a
runIOGen g = fst . runGen g <$> newStdGen

genr :: Int -> Int -> Gen Int
genr a b = state $ uniformR (a, b-1)

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

instance Indexable VS.Vector where
  index = (VS.!)
  size = VS.length

choose :: Indexable c => c a -> Gen a
choose xs = let n = size xs in index xs <$> genr 0 n

genuntil :: (a -> Bool) -> Gen a -> Gen a
genuntil p g = do
  x <- g
  if p x
    then pure x
    else genuntil p g

genpair :: (Int -> Int -> Bool) -> Gen Int -> Gen (Int, Int)
genpair f g = genuntil (uncurry f) $ liftA2 (,) g g

distribute :: Int -> Int -> Int -> Gen (Vector Int)
distribute _ _ low | low < 0 = error "low must be non-negative"
distribute _ n _ | n <= 0 = error "n must be positive"
distribute s n 0 = runGenST do
  v <- VM.generateM (n + 1) \i ->
    if
      | i == 0 -> pure 0
      | i == n -> pure s
      | otherwise -> liftg (genr 0 s)
  VA.sort v
  delimeters <- V.unsafeFreeze v
  pure $ V.zipWith (-) (V.drop 1 delimeters) delimeters
distribute s n low = V.map (+ low) <$> distribute (s - low * n) n 0

shuffle :: Vector Int -> Gen (Vector Int)
shuffle v' = runGenST do
  let n = V.length v'
  let go v i
        | i == n = V.unsafeFreeze v
        | otherwise = do
            j <- liftg $ genr i n
            VM.swap v i j
            go v (i + 1)
  v <- V.thaw v'
  go v 0

genperm :: Int -> Gen (Vector Int)
genperm n = shuffle $ V.enumFromN 0 n

gendistinct :: (Ord a) => Int -> Gen a -> Gen (Vector a)
gendistinct n g = runGenST do
  v <- VM.new n
  let go i seen v
        | i == n = V.unsafeFreeze v
        | otherwise = do
            x <- liftg $ genuntil (not . flip Set.member seen) g
            VM.write v i x
            go (i + 1) (Set.insert x seen) v
  go 0 Set.empty v
