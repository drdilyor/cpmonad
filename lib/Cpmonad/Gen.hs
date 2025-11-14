{-# LANGUAGE NoFieldSelectors #-}

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
  Indexable (..),
  choose,
  genpair,
  distribute,
  shuffle,
  genperm,
  gendistinct,
  GraphOptions (..),
  connected,
  disconnected,
  gentree,
  treeFromPruferCode,
  gengraph,
  vi,
) where

import Control.Monad
import Control.Monad.ST
import Control.Monad.State.Strict (State, StateT, lift, runState, runStateT, state)
import Data.Coerce
import Data.Graph.Haggle
import Data.Graph.Haggle.BiDigraph
import Data.List
import Data.Maybe (fromJust)
import Data.STRef
import Data.Set qualified as Set
import Data.Vector (Vector, (!))
import Data.Vector qualified as V
import Data.Vector.Algorithms.Merge qualified as VA
import Data.Vector.Mutable qualified as VM
import Data.Vector.Strict qualified as VS
import Debug.Trace
import GHC.Stack (HasCallStack)
import System.Random (StdGen, newStdGen, uniformR)
import Unsafe.Coerce (unsafeCoerce)

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
genri a b = genr a (b - 1)

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
      | otherwise -> liftg (genr 0 s)
  VA.sort v
  delimeters <- V.unsafeFreeze v
  pure $ V.zipWith (-) (V.drop 1 delimeters) delimeters
distribute s n low = V.map (+ low) <$> distribute (s - low * n) n 0

-- | Generate a shuffled version of the vector
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

-- data Graph v e = Graph !(Vector v) !(Vector (Int, Int, e))
--   deriving (Eq, Show)

data GraphOptions = GraphOptions
  { connected :: Bool
  , allowLoops :: Bool
  , allowMulti :: Bool
  , allowAntiparallel :: Bool
  , acyclic :: Bool
  }

disconnected :: GraphOptions
disconnected =
  GraphOptions
    { connected = False
    , allowLoops = False
    , allowMulti = False
    , allowAntiparallel = False
    , acyclic = False
    }

connected :: GraphOptions
connected = disconnected{connected = False}

gentree :: Int -> Gen BiDigraph
gentree n = treeFromPruferCode <$> V.replicateM (n - 2) (genr 0 n)

vi :: Int -> Vertex
vi = unsafeCoerce

treeFromPruferCode :: Vector Int -> BiDigraph
treeFromPruferCode code = runST do
  let n = V.length code + 2
  g <- newSizedMBiDigraph n (n - 1)
  degree <- VM.replicate n (1 :: Int)
  leaves <- newSTRef Set.empty

  V.forM_ code $ VM.modify degree (+ 1)
  replicateM_ n $ addVertex g

  forM_ [0 .. n - 1] \i -> do
    di <- VM.read degree i
    when (di == 1) $ modifySTRef' leaves (Set.insert (vi i))

  let
    go i leaves
      | i == n - 2 =
          void $ addEdge g (Set.findMin leaves) (Set.findMax leaves)
    go i (Set.minView -> Just (leaf, leaves)) = do
      let v = code V.! i
      _ <- addEdge g leaf (vi v)
      VM.modify degree (subtract 1) v
      di <- VM.read degree v
      go (i + 1) $ if di == 1 then Set.insert (vi v) leaves else leaves
    go _ _ = error "infallible"
  -- collect vertices with degree == 1
  leaves <- VM.ifoldl' (\cases s i 1 -> Set.insert (vi i) s; s _ _ -> s) Set.empty degree
  go 0 leaves

  freeze g

gengraph :: GraphOptions -> Digraph
gengraph _ = undefined
