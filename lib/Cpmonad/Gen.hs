module Cpmonad.Gen(
  Gen,
  lift,
  state,
  runGen,
  genr,
  genri,
  Indexable(..),
  choose,
  genpair,
  distribute,
  shuffle,
  genperm,
  gendistinct,
) where

import Control.Monad
import Control.Monad.ST
import Control.Monad.State.Strict (StateT (..), state, lift)
import Data.List
import Data.Set qualified as Set
import Data.Vector (Vector)
import Data.Vector qualified as V
import Data.Vector.Mutable qualified as VM
import Data.Vector.Strict qualified as VS
import Data.Vector.Algorithms.Merge qualified as VA
import System.Random (uniformR, StdGen)

type Gen s = StateT StdGen (ST s)

runGen :: (forall s. Gen s a) -> StdGen -> (a, StdGen)
runGen g s = runST $ runStateT g s

genr :: Int -> Int -> Gen s Int
genr a b = state $ uniformR (a, b-1)

genri :: Int -> Int -> Gen s Int
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

choose :: Indexable c => c a -> Gen s a
choose xs = let n = size xs in index xs <$> genr 0 n

genuntil :: (a -> Bool) -> Gen s a -> Gen s a
genuntil p g = do
  x <- g
  if p x
    then pure x
    else genuntil p g

genpair :: (Int -> Int -> Bool) -> Gen s Int -> Gen s (Int, Int)
genpair f g = genuntil (uncurry f) $ liftA2 (,) g g

distribute :: Int -> Int -> Int -> Gen s (Vector Int)
distribute _ _ low | low < 0 = error "low must be non-negative"
distribute _ n _ | n <= 0 = error "n must be positive"
distribute s n 0 = do
  v <- VM.replicateM (n - 1) $ genr 0 s
  VA.sort v
  v' <- V.unsafeFreeze v
  let delimeters = V.singleton 0 <> v' <> V.singleton s
  pure $ V.zipWith (-) (V.drop 1 delimeters) delimeters
distribute s n low = V.map (+ low) <$> distribute (s - low * n) n 0

shuffle' :: VM.STVector s Int -> Gen s (Vector Int)
shuffle' v = do
  let n = VM.length v
  forM_ [0 .. n - 1] \i -> do
    j <- genr i n
    x <- VM.read v i
    VM.write v j x
  V.unsafeFreeze v

shuffle :: Vector Int -> Gen s (Vector Int)
shuffle v = shuffle' =<< V.thaw v

genperm :: Int -> Gen s (Vector Int)
genperm n = shuffle' =<< V.unsafeThaw (V.enumFromN 0 n)

gendistinct :: (Ord a) => Int -> Gen s a -> Gen s (Vector a)
gendistinct n g = VM.new n >>= go 0 Set.empty
  where
    go i seen v =
      if i == n
        then V.unsafeFreeze v
        else do
          x <- genuntil (not . flip Set.member seen) g
          VM.write v i x
          go (i + 1) (Set.insert x seen) v
