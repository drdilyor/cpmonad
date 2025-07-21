{-# LANGUAGE NoFieldSelectors #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
module Main where

import Control.Monad (forM, forM_)
import Data.ByteString.Char8(ByteString)
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Builder as B
import Data.Functor (($>))
import Data.List
import Data.Vector (Vector, (!))
import Data.Vector qualified as V
import Data.Vector.Mutable qualified as VM
import Lens.Micro
import Lens.Micro.TH
import System.Random (uniformR, StdGen, mkStdGen)
import qualified Data.Vector.Strict
import Control.Monad.ST

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
    printerA :: Printer a
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


data Printer a = Printer
  { toPrinted :: (a, B.Builder) -> B.Builder,
    fromPrinted :: (a, ByteString) -> Maybe (a, ByteString)
  }

instance Semigroup (Printer a) where
  p1 <> p2 = Printer {..}
    where
      toPrinted xb = p1.toPrinted xb <> p2.toPrinted xb
      fromPrinted xs = p1.fromPrinted xs >>= p2.fromPrinted


readInt :: Lens' a Int -> Printer a
readInt f = Printer {..}
  where
    toPrinted (!x, !b) = b <> B.intDec (x ^. f)
    fromPrinted (!x, !s) = do
      (n, s') <- B.readInt s
      pure (x & f .~ n, s')

readChar :: Char -> Printer a
readChar c = Printer {..}
  where
    toPrinted (!_, !b) = b <> B.char8 c
    fromPrinted (!x, !s) = do
      c' <- s `B.indexMaybe` 0
      if c' == c then Just (x, B.tail s) else Nothing

readVecInt :: Lens' a Int -> Lens' a (Vector Int) -> Printer a
readVecInt n f = Printer {..}
  where
    toPrinted (!x, !b) =
      -- assert $ x ^. n == V.length (x ^. f)
      if V.length (x ^. f) == 0
        then b
        else
          V.foldl' (\a el -> a <> B.intDec el <> B.char8 ' ') b (V.init (x ^. f))
            <> B.intDec (V.last $ x ^. f)
    fromPrinted (!x, !s') =
      let count = x ^. n
      in if count == 0 then Just (x & f .~ V.empty, s') else runST do
        v <- VM.new (x ^. n)
        let go !s !i
              | i == count - 1 =
                case B.readInt s of
                  Nothing -> pure Nothing
                  Just (num, s') -> VM.write v i num >> pure (Just s')
              | otherwise =
                case B.readInt s of
                  Nothing -> pure Nothing
                  Just (num, s') ->
                    case s' `B.indexMaybe` 0 of
                      Just ' ' -> VM.write v i num >> go (B.tail s') (i+1)
                      _ -> pure Nothing
        res <- go s' 0
        v' <- V.freeze v
        pure $ (x & f .~ v',) <$> res


type Input = Vector Int
type Output = Maybe Int

n :: Lens' Input Int
n = lens V.length (\_ n -> V.replicate n 0)

arr :: Lens' Input (Vector Int)
arr = lens id (const id)

output :: Lens' Output Int
output = non (-1)

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
      genAll :: [Gen a] -> [a]
  let p =
        Problem
          { tests = map (\x -> (x, model x)) (genAll (
              map (pure . V.fromList)
                [ [1, 3, 2, 5],
                  [1, 100],
                  [298077099, 766294630, 440423914, 59187620, 725560241, 585990757, 965580536, 623321126, 550925214, 917827435]
                ]
              <> replicate 20 (gen1 20 (10^9))
              <> replicate 10 (gen1 20_000 (10^9))
              <> replicate 10 (gen1 20_000 (10^9))
              )),
            sols = [sol1, sol1, sol1, sol2],
            check = const (==),
            printerI = readInt n <> readChar '\n' <> readVecInt n arr,
            printerO = readInt output,
            printerA = readInt output
          }
  runProblem p
