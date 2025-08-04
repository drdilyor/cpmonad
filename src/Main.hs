{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
{-# LANGUAGE TemplateHaskell #-}
module Main where

import Control.DeepSeq (deepseq, force, NFData)
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
import System.Timeout (timeout)
import Control.Exception.Base (evaluate)

runProblem :: (NFData a, NFData i, Eq i, Eq a, Show i, Show a, NFData o) => Problem i o a -> IO ()
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
    -- putStrLn $ "test " <> show ix
    -- putStrLn $ "  " <> show (tests !! ix)
    -- hFlush stdout
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
        o' <- timeout 100_000 (evaluate . force $ f i)
        case o' of
          Nothing -> putStr "T" $> (TLE, (i, a, eo))
          Just o -> if check i a o
            then putStr "." $> (AC 1, (i, a, o))
            else putStr "X" $> (WA, (i, a, o))
      putStrLn ""
      pure verdict

  forM_ (zip [0 :: Int ..] verdicts) \case
    (ix, (AC x, _)) -> do
      putStrLn $ "sol " <> show ix <> ": AC " <> show (round (x * 100) :: Int) <> "%"
    (ix, (_, (i, a, o))) -> do
      putStrLn $ "sol " <> show ix <> ": WA:"
      B.putStrLn $ ">>> input:\n" <> (B.take 100 . B.toStrict . B.toLazyByteString . fromJust $ printerI.toPrinted i) <> "\n"
      B.putStrLn $ ">>> output:\n" <> (B.take 100 . B.toStrict . B.toLazyByteString . fromJust $ printerO.toPrinted o) <> "\n"
      B.putStrLn $ ">>> test output:\n" <> (B.take 100 . B.toStrict . B.toLazyByteString . fromJust $ printerA.toPrinted a) <> "\n"

data Input = Input
  { _n :: Int,
    _arr :: Vector Int,
    _q :: Int,
    _queries :: Vector (Int, Int)
  }
  deriving (Show, Eq, Generic, NFData)

type Output = (Int, Vector Int)

makeLenses ''Input

sol1 :: Input -> Output
sol1 (Input _ arr _ queries) =
  let pref = V.scanl' (+) 0 arr
      ans = flip V.map queries \(l, r) ->
        pref ! (r + 1) - pref ! l
   in (V.length ans, ans)

sol2 :: Input -> Output
sol2 (Input _ arr _ queries) =
  let ans = flip V.map queries \(l, r) ->
        V.sum (V.slice l (r - l + 1) arr)
   in (V.length ans, ans)

gen1 :: Int -> Int -> Gen Input
gen1 n q =
  Input n
    <$> V.replicateM n (genr 0 (10^9))
    <*> pure q
    <*> V.replicateM q do
      l <- genr 0 n
      r <- genr 0 n
      if l <= r
      then pure (l, r)
      else pure (r, l)

gen2 :: Int -> Int -> Gen Input
gen2 = gen1

model :: Input -> Output
model = sol1

genAll xs = fst $ runGen (sequence xs) (mkStdGen 1)
genAll :: [Gen a] -> [a]

p :: Problem Input Output Output
p =
  Problem
    { tests = map (\x -> (x, model x)) (genAll (
        map (\(v, u) -> pure $ Input (length v) (V.fromList v) (length u) (V.fromList u))
          [ ([1, 3, 2, 5],[(0,1)]),
            ([1, 3, 2, 5],[(0,2)]),
            ([1, 3, 2, 5],[(0,3)])
          ]
        <> replicate 1 (gen1 20 20)
        <> replicate 1 (gen1 2000 2000)
        <> replicate 1 (gen1 20000 20000)
        <> replicate 1 (gen1 200000 200000)
        )),
      sols = [sol1, sol1, sol1, sol2],
      check = const (==),
      printerI = pint n <> endl
                <> pvec ' ' n arr 0 (pint idl) <> endl
                <> pint q <> endl
                <> pvec ' ' q queries (0,0) (pint _1 <> sp <> pint _2),
      printerO = pint _1 <> endl <> pvecint ' ' _1 _2,
      printerA = pint _1 <> endl <> pvecint ' ' _1 _2,
      ei = Input 0 V.empty 0 V.empty, eo = (0, V.empty), ea = (0, V.empty)
    }

main :: IO ()
main = runProblem p
