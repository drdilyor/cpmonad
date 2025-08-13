{-# LANGUAGE NoFieldSelectors #-}

module Cpmonad(
  VerdictBad(..),
  Verdict(..),
  wa,
  ac,
  mkPts,
  getPoints,
  hasPoints,
  mergeVerdict',

  Problem(..),

  runSolutions,
  generateTests',

  module Cpmonad.Gen,
  module Cpmonad.Printer,
  module Cpmonad.Misc,

  Default,
  NFData,
  Generic,
) where

import Control.DeepSeq
import Control.Exception
import Control.Monad
import Data.ByteString.Builder qualified as B
import Data.ByteString.Char8 qualified as B
import Data.Default
import Data.List
import Data.Maybe (fromJust)
import GHC.Generics (Generic)
import System.CPUTime
import System.Directory
import System.IO
import System.IO.Error (isDoesNotExistError)
import System.Timeout (timeout)

import Cpmonad.Gen
import Cpmonad.Printer
import Cpmonad.Misc

-- [[[ Basics ]]]

-- Pts x where x must be between 0 and 1
data Verdict = Pts Float | Bad VerdictBad deriving (Show)

data VerdictBad = RE | TLE | Other String deriving (Show)

wa :: Verdict
wa = Pts 0

ac :: Verdict
ac = Pts 1

-- clamp the points
mkPts :: Float -> Verdict
mkPts x = Pts (0 `max` x `min` 1)

getPoints :: Verdict -> Float
getPoints (Pts x) = x
getPoints _ = 0

hasPoints :: Verdict -> Bool
hasPoints x = getPoints x > 0

mergeVerdict' :: (Verdict, b) -> (Verdict, b) -> (Verdict, b)
mergeVerdict' a b = case (a, b) of
  ((Pts x, _), (Pts y, _))
    | x < y -> a
    | otherwise -> b
  ((Pts _, _), _) -> b
  (_, (Pts _, _)) -> a
  (_, _) -> a

instance Semigroup Verdict where
  a <> b = fst $ mergeVerdict' (a, ()) (b, ())

instance Monoid Verdict where
  mempty = Pts 1

data Problem i a o where
  Problem ::
    (NFData i, NFData a, NFData o,
     Default i, Default a, Default o,
     Eq i, Eq a, Eq o) =>
    { tests :: [(i, a)],
      sols :: [i -> o],
      check :: i -> a -> o -> Bool,
      printerI :: Printer i,
      printerA :: Printer (i, a),
      printerO :: Printer (i, o)
    } ->
    Problem i a o

wrapAction :: String -> IO a -> IO a
wrapAction name step = do
  putStrLn $ ":: " <> name <> " ..."
  start <- getCPUTime
  res <- step
  end <- getCPUTime
  let diffms = (round :: Double -> Int) $ fromIntegral (end - start) / (10 ^ (9 :: Int))
  putStrLn $ "   took " <> show diffms <> "ms"
  pure res

generateTests' :: Problem i a o -> IO ()
generateTests' Problem {..} = do
  wrapAction "cleaning up" do
    catchJust (\e -> if isDoesNotExistError e then Just e else Nothing)
      (removeDirectoryRecursive "tests")
      (const $ pure ())
    createDirectoryIfMissing False "tests"

  _ <- wrapAction "evaluating tests" $ evaluate $ force tests

  wrapAction "outputting tests" do
    forM_ (zip [0 :: Int ..] tests) \(ix, (i, a)) -> do
      h <- openBinaryFile ("tests/" <> show ix <> ".in") WriteMode
      hSetBuffering h (BlockBuffering $ Just 4096)
      B.hPutBuilder h . fromJust $ printerI.toPrinted i
      hClose h

      h <- openBinaryFile ("tests/" <> show ix <> ".out") WriteMode
      hSetBuffering h (BlockBuffering $ Just 4096)
      B.hPutBuilder h . fromJust $ printerA.toPrinted (i, a)
      hClose h

  wrapAction "parsing outputs" do
    forM_ (zip [0 :: Int ..] tests) \(ix, _) -> do
      -- TODO
      s <- B.readFile ("tests/" <> show ix <> ".in")
      let i = fst . fromJust $ printerI.fromPrinted (def, s)
      s <- B.readFile ("tests/" <> show ix <> ".out")
      let a = fst <$> printerA.fromPrinted ((i, def), s)
      _ <- evaluate $ force (i, a)
      pure ()

  -- only for checking the performance of the printer itself
  wrapAction "transcoding tests" do
    forM_ (zip [0 :: Int ..] tests) \(_, (i, a)) -> do
      let i' = fmap fst . printerI.fromPrinted . (def,) . B.toStrict . B.toLazyByteString . fromJust $ printerI.toPrinted i
      let a' = do i <- i'
                  fmap (snd . fst) . printerA.fromPrinted . ((i, def),) . B.toStrict . B.toLazyByteString . fromJust $ printerA.toPrinted (i, a)
      when (i' /= Just i) $ putStrLn "input didn't match!!!"
      when (a' /= Just a) $ putStrLn "aux didn't match!!!"

runSolutions :: Problem i a o -> IO ()
runSolutions Problem {..} = do
  verdicts <- wrapAction "running" do
    forM (zip [0 :: Int ..] sols) \(ix, f) -> do
      putStr $ "sol " <> show ix <> ": "
      verdict <- foldl1' mergeVerdict' <$> forM tests \(i, a) -> do
        o' <- timeout 100_000 (evaluate . force $ f i)
        case o' of
          Nothing -> putStr "T" >> pure (Bad TLE, (i, a, Nothing))
          Just o -> if check i a o
            then putStr "." >> pure (Pts 1, (i, a, Just o))
            else putStr "X" >> pure (Pts 0, (i, a, Just o))
      putStrLn ""
      pure verdict

  wrapAction "evaluating" do
    forM_ (zip [0 :: Int ..] verdicts) \case
      (ix, (Pts x, _)) | x > 0 -> do
        putStrLn $ "sol " <> show ix <> ": Pts " <> show (round (x * 100) :: Int) <> "%"
      (ix, (_, (i, a, o))) -> do
        putStrLn $ "sol " <> show ix <> ": WA:"
        B.putStrLn $ ">>> input:\n" <> (B.take 100 . B.toStrict . B.toLazyByteString . fromJust $ printerI.toPrinted i) <> "\n"
        case o of
          Just o -> B.putStrLn $ ">>> output:\n" <> (B.take 100 . B.toStrict . B.toLazyByteString . fromJust $ printerO.toPrinted (i, o)) <> "\n"
          _ -> pure ()
        B.putStrLn $ ">>> test output:\n" <> (B.take 100 . B.toStrict . B.toLazyByteString . fromJust $ printerA.toPrinted (i, a)) <> "\n"
