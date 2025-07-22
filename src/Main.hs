{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
module Main where

import Control.DeepSeq (deepseq, NFData)
import Control.Exception (catchJust)
import Control.Monad (forM, forM_, when)
import Data.ByteString.Builder qualified as B
import Data.ByteString.Char8 qualified as B
import Data.Vector (Vector, (!))
import Data.Vector qualified as V
import Data.Vector.Algorithms.Merge


import Lens.Micro
import System.CPUTime (getCPUTime)
import System.Directory
import System.IO
import System.IO.Error (isDoesNotExistError)

import Cpmonad
import Data.Functor (($>))
import System.Random (mkStdGen)
import Data.List (foldl1')

runProblem :: (Show i, Show o, Show a, NFData a, NFData i, Eq i, Eq a) => Problem i o a -> IO ()
runProblem (Problem {..}) = do
  -- output tests
  catchJust (\e -> if isDoesNotExistError e then Just e else Nothing)
    (removeDirectoryRecursive "tests")
    (const $ pure ())

  createDirectoryIfMissing False "tests"

  putStr "generating tests ...  "
  start <- tests `deepseq` getCPUTime
  forM_ (zip [0 :: Int ..] tests) \(ix, (i, a)) -> do
    h <- openBinaryFile ("tests/" <> show ix <> ".in") WriteMode
    hSetBuffering h (BlockBuffering $ Just 4096)
    B.hPutBuilder h $ printerI.toPrinted (i, mempty)
    hClose h

    h <- openBinaryFile ("tests/" <> show ix <> ".out") WriteMode
    hSetBuffering h (BlockBuffering $ Just 4096)
    B.hPutBuilder h $ printerA.toPrinted (a, mempty)
    hClose h
  end <- getCPUTime
  let diffms = round $ fromIntegral (end - start) / (10^9)
  putStrLn $ "took " <> show diffms <> "ms"

  putStr "parsing tests ...     "
  start <- getCPUTime
  forM_ (zip [0 :: Int ..] tests) \(ix, _) -> do
    s <- B.readFile ("tests/" <> show ix <> ".in")
    let i = fst <$> printerI.fromPrinted (ei, s)
    s <- B.readFile ("tests/" <> show ix <> ".out")
    let a = fst <$> printerA.fromPrinted (ea, s)
    last (show i) `deepseq` last (show a) `deepseq` pure ()
  end <- getCPUTime
  let diffms = round $ fromIntegral (end - start) / (10^9)
  putStrLn $ "took " <> show diffms <> "ms"

  putStr "transcoding tests ... "
  start <- getCPUTime
  forM_ (zip [0 :: Int ..] tests) \(_, (i, a)) -> do
    let i' = fmap fst . printerI.fromPrinted . (ei,) . B.toStrict . B.toLazyByteString $ printerI.toPrinted (i, mempty)
    let a' = fmap fst . printerA.fromPrinted . (ea,) . B.toStrict . B.toLazyByteString $ printerA.toPrinted (a, mempty)
    when (i' /= Just i) $ putStrLn "input didn't match!!!"
    when (a' /= Just a) $ putStrLn "aux didn't match!!!"
  end <- getCPUTime
  let diffms = round $ fromIntegral (end - start) / (10^9)
  putStrLn $ "took " <> show diffms <> "ms"

  -- run solutions
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

type Input = Vector Int
type Output = Maybe Int

veclen :: Lens' (Vector Int) Int
veclen = lens V.length (\_ n -> V.replicate n 0)

sortVec :: Ord a => Vector a -> Vector a
sortVec v' =
  V.create do
    v <- V.thaw v'
    sort v
    pure v

sol1 :: Input -> Output
sol1 v' =
  let x1 = V.head v'
      x2 = V.last v'
      n = V.length v'
      v = sortVec v'
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
      v = sortVec v'
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
              <> replicate 10 (gen1 200_000 (10^9))
              )),
            sols = [sol1, sol1, sol1, sol2],
            check = const (==),
            printerI = nest veclen readInt <> readChar '\n' <> readVecInt,
            printerO = nest (non (-1)) readInt,
            printerA = nest (non (-1)) readInt,
            ei = V.empty, eo = Nothing, ea = Nothing
          }
  runProblem p
