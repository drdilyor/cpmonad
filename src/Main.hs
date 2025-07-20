{-# OPTIONS_GHC -Wno-type-defaults #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
module Main where

import Control.Applicative (asum)
import Control.Monad (forM, forM_, replicateM)
import Control.Monad.State.Strict
import Data.Functor (($>))
import Data.List
import Data.Vector (Vector, (!))
import Data.Vector qualified as V
import Data.Function (on)
import System.Random (uniformR, StdGen, mkStdGen)
import qualified Data.Vector.Strict

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
    check :: i -> a -> o -> Bool
  }


runProblem :: (Show i, Show o, Show a) => Problem i o a -> IO ()
runProblem (Problem {..}) = do
  verdicts <-
    forM (zip [0 :: Int ..] sols) \(ix, f) -> do
      putStr $ "sol " <> show ix <> ": "
      verdict <- foldl1' mergeVerdict' <$> forM tests \(i, a) -> do
        let o = f i
        if check i a o
          then putStr "." $> (AC 1, (i, a, o))
          else putStr "X" $> (WA, (i, a, o))
      putStrLn ""
      pure verdict

  forM_ (zip [0 :: Int ..] verdicts) \case
    (ix, (AC x, _)) -> do
      putStrLn $ "sol " <> show ix <> ": AC " <> show (round (x * 100) :: Int) <> "%"
    (ix, (_, (i, a, o))) -> do
      putStrLn $ "sol " <> show ix <> ": WA:"
      putStrLn $ "  i=" <> show i
      putStrLn $ "  o=" <> show o
      putStrLn $ "  a=" <> show a

newtype Gen a = Gen { runGen :: StdGen -> (a, StdGen) } deriving (Functor)

instance Applicative Gen where
  pure x = Gen $ (x,)
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

type Input = Vector Int
type Output = Maybe Int

sol1 :: Input -> Output
sol1 v' =
  let x1 = V.head v'
      x2 = V.last v'
      n = V.length v'
      v = V.fromList . sort . V.toList $ v'
      fd l r x =
        if r - l <= 1
          then l
          else
            let mid = (l + r) `div` 2
             in if v ! mid <= x then fd mid r x else fd l mid x

      go x s | x >= x2 = Just s
      go x s =
        let x' = fd (-1) n (x * 2)
         in if x' == -1 || x' == x
              then Nothing
              else go x' (s + 1)
   in go x1 0

sol2 :: Input -> Output
sol2 v' =
  let x1 = V.head v'
      x2 = V.last v'
      v = V.fromList . sort . V.toList $ v'
  in liftA2 (-) (V.findIndex (== x2) v) (V.findIndexR (== x1) v)

gen1 :: Int -> Int -> Gen (Vector Int)
gen1 n m = V.replicateM n (genr 0 m)

gen2 :: Int -> Int -> Gen (Vector Int)
gen2 n x = do
  a <- genr 0 x
  b <- genr (a*2+1) (x*2+1)
  pure $ V.replicate (n-1) a `V.snoc` b

main :: IO ()
main = do
  let model = sol1
  let genAll xs = fst $ runGen (sequence xs) (mkStdGen 1)
      genAll :: [Gen Input] -> [Input]
  let p =
        Problem
          { tests = map (\x -> (x, model x)) $ genAll (
              map (pure . V.fromList)
                [ [1, 3, 2, 5],
                  [1, 100],
                  [298077099, 766294630, 440423914, 59187620, 725560241, 585990757, 965580536, 623321126, 550925214, 917827435]
                ]
              <> replicate 20 (gen1 20 (10^9))
              <> replicate 10 (gen1 20_000 (10^9))
              <> replicate 10 (gen1 20_000 (10^9))
              ),
            sols = [sol1, sol1, sol1, sol2],
            check = const (==)
          }
  runProblem p

