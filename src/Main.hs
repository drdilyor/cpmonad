module Main where

import Control.Applicative (asum)
import Control.Monad (forM, forM_)
import Data.Functor (($>))
import Data.List
import Data.Vector (Vector, (!))
import Data.Vector qualified as V
import Data.Function (on)

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
      putStrLn $ "sol " <> show ix <> ": AC " <> show (round (x * 100) :: Int)
    (ix, (_, (i, a, o))) -> do
      putStrLn $ "sol " <> show ix <> ": WA:"
      putStrLn $ "  i=" <> show i
      putStrLn $ "  o=" <> show o
      putStrLn $ "  a=" <> show a

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

main :: IO ()
main = do
  let model = sol1
  let p =
        Problem
          { tests =
              map
                ((\x -> (x, model x)) . V.fromList)
                [ [1, 3, 2, 5],
                  [1, 100],
                  [298077099, 766294630, 440423914, 59187620, 725560241, 585990757, 965580536, 623321126, 550925214, 917827435]
                ],
            sols = [sol1, sol1, sol1, sol2],
            check = const (==)
          }
  runProblem p
