module Cpmonad.Gen (
  Gen,
  GenST,
  runGen,
  runGenST,
  state,
  liftg,
  runIOGen,
  genr,
  genri,
  genrw,
  Indexable (..),
  choose,
  genuntil,
  genpair,
  distribute,
  shuffle,
  genperm,
  gendistinct,
) where

import Control.Monad
import Control.Monad.ST
import Control.Monad.State.Strict (State, StateT, runState, runStateT, state)
import Data.List
import Data.Set qualified as Set
import Data.Vector (Vector, (!))
import Data.Vector qualified as V
import Data.Vector.Algorithms.Merge qualified as VA
import Data.Vector.Mutable qualified as VM
import Data.Vector.Strict qualified as VS
import System.Random (StdGen, newStdGen, uniformR)

-- | Monad where usual generators live in
type Gen = State StdGen

-- | Monad when you want to do mutations for faster generation algorithms
type GenST s = StateT StdGen (ST s)

runGen :: Gen a -> StdGen -> (a, StdGen)
runGen = runState

-- | Convert 'GenST' to 'Gen'
runGenST :: (forall s. GenST s a) -> Gen a
runGenST g = state \s -> runST $ runStateT g s

liftg :: Gen a -> GenST s a
liftg = state . runState

-- | Seed the generator with 'newStdGen'
runIOGen :: Gen a -> IO a
runIOGen g = fst . runGen g <$> newStdGen

-- | Int between [a, b)
genr :: Int -> Int -> Gen Int
genr a b = state $ uniformR (a, b - 1)

-- | Int between [a, b]
genri :: Int -> Int -> Gen Int
genri a b = genr a (b + 1)

genrw :: Int -> Int -> Int -> Gen Int
genrw l r w
  | w > 0 = maximum <$> replicateM w (genr l r)
  | w < 0 = minimum <$> replicateM (-w) (genr l r)
  | otherwise = genr l r

-- | Class of types that can be used with 'choose'
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

-- | Choose an element of a collection
choose :: (Indexable c) => c a -> Gen a
choose xs = let n = size xs in index xs <$> genr 0 n

-- | Keep generating until predicate returns True. Stops after one million tries
genuntil :: (a -> Bool) -> Gen a -> Gen a
genuntil p g = go (1000_000 :: Int)
 where
  go 0 = error "genuntil: not found after a million tries"
  go i = do
    x <- g
    if p x
      then pure x
      else go (i - 1)

{- | Generate a pair of integers satisfying the binary predicate f.

Useful as @genpair (<=)@
-}
genpair :: (Int -> Int -> Bool) -> Gen Int -> Gen (Int, Int)
genpair f g = genuntil (uncurry f) $ liftA2 (,) g g

{- | Given @s@, generate @n@ numbers such that their sum adds up to @s@

Can be used to generate the parameters of the testcases.
-}
distribute
  :: Int
  -- ^ total sum @s@
  -> Int
  -- ^ number of numbers @n@
  -> Int
  -- ^ minimum number to generate
  -> Gen (Vector Int)
distribute _ n _ | n <= 0 = error "n must be positive"
distribute s n 0 = runGenST do
  v <- VM.generateM (n + 1) \i ->
    if
      | i == 0 -> pure 0
      | i == n -> pure s
      | otherwise -> liftg (genri 0 s)
  VA.sort v
  delimeters <- V.unsafeFreeze v
  pure $ V.zipWith (-) (V.drop 1 delimeters) delimeters
distribute s n low = V.map (+ low) <$> distribute (s - low * n) n 0

-- | Generate a shuffled version of the vector
shuffle :: Vector a -> Gen (Vector a)
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

-- | Generate a permutation
genperm :: Int -> Gen (Vector Int)
genperm n = shuffle $ V.enumFromN 0 n

-- | Generate n distinct @a@'s
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
