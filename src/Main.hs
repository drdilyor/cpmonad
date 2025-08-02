{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
{-# LANGUAGE TemplateHaskell #-}
module Main where

import Control.DeepSeq (deepseq, NFData)
import Control.Exception (catchJust)
import Control.Monad (forM, forM_, when)
import Data.ByteString.Builder qualified as B
import Data.ByteString.Char8 qualified as B
import Data.Functor (($>))
import Data.List (foldl1')
import Data.Vector (Vector, (!))
import Data.Vector qualified as V
import Data.Vector.Algorithms.Merge
import Lens.Micro
import Lens.Micro.TH
import System.CPUTime (getCPUTime)
import System.Directory
import System.IO
import System.IO.Error (isDoesNotExistError)
import System.Random (mkStdGen)

import Cpmonad
import Printer
import Data.Maybe (fromJust)
import GHC.Generics (Generic)

runProblem :: (NFData a, NFData i, Eq i, Eq a, Show i) => Problem i o a -> IO ()
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
    B.hPutBuilder h . fromJust $ printerI.toPrinted i
    hClose h

    h <- openBinaryFile ("tests/" <> show ix <> ".out") WriteMode
    hSetBuffering h (BlockBuffering $ Just 4096)
    B.hPutBuilder h . fromJust $ printerA.toPrinted a
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
    i `deepseq` a `deepseq` pure ()
  end <- getCPUTime
  let diffms = round $ fromIntegral (end - start) / (10^9)
  putStrLn $ "took " <> show diffms <> "ms"

  putStr "transcoding tests ... "
  start <- getCPUTime
  forM_ (zip [0 :: Int ..] tests) \(_, (i, a)) -> do
    let i' = fmap fst . printerI.fromPrinted . (ei,) . B.toStrict . B.toLazyByteString . fromJust $ printerI.toPrinted i
    let a' = fmap fst . printerA.fromPrinted . (ea,) . B.toStrict . B.toLazyByteString . fromJust $ printerA.toPrinted a
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
      B.putStrLn $ ">>> input:\n" <> (B.toStrict . B.toLazyByteString . fromJust $ printerI.toPrinted i) <> "\n"
      B.putStrLn $ ">>> output:\n" <> (B.toStrict . B.toLazyByteString . fromJust $ printerO.toPrinted o) <> "\n"
      B.putStrLn $ ">>> test output:\n" <> (B.toStrict . B.toLazyByteString . fromJust $ printerA.toPrinted a) <> "\n"

data Input = Input { _n :: Int, _arr :: Vector Int } deriving (Show, Eq, Generic, NFData)
type Output = Maybe Int

makeLenses ''Input

sortVec :: Ord a => Vector a -> Vector a
sortVec v' =
  V.create do
    v <- V.thaw v'
    sort v
    pure v

sol1 :: Input -> Output
sol1 (Input _ v') =
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
sol2 (Input _ v') =
  let x1 = V.head v'
      x2 = V.last v'
      v = sortVec v'
  in liftA2 (-) (V.findIndex (== x2) v) (V.findIndexR (== x1) v)

gen1 :: Int -> Int -> Gen Input
gen1 n m = Input n <$> V.replicateM n (genr 0 m)

gen2 :: Int -> Int -> Gen Input
gen2 n x = do
  a <- genr 0 x
  b <- genr (a*2+1) (x*2+1)
  pure $ Input n $ V.replicate (n-1) a `V.snoc` b

main :: IO ()
main = do
  let model = sol1
  let genAll xs = fst $ runGen (sequence xs) (mkStdGen 1)
      genAll :: [Gen a] -> [a]
  let p =
        Problem
          { tests = map (\x -> (x, model x)) (genAll (
              map (\v -> pure . Input (length v) $ V.fromList v)
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
            printerI = pint n <> endl <> pvec ' ' n arr 0 (pint idl),
            printerO = pint (non (-1)),
            printerA = pint (non (-1)),
            ei = Input 0 V.empty, eo = Nothing, ea = Nothing
          }
  runProblem p
