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

  vsort,
  vinverse,
  runSolutions,
  generateTests',

  Default,
  NFData,
) where

import Control.DeepSeq
import Control.Exception
import Control.Monad
import Control.Monad.ST
import Control.Monad.State.Strict (StateT (..), state, lift)
import Data.ByteString.Builder qualified as B
import Data.ByteString.Char8 qualified as B
import Data.Default
import Data.List
import Data.Maybe (fromJust)
import Data.Set qualified as Set
import Data.Vector (Vector)
import Data.Vector qualified as V
import Data.Vector.Mutable qualified as VM
import Data.Vector.Strict qualified as VS
import Data.Vector.Algorithms.Merge qualified as VA
import System.CPUTime
import System.Directory
import System.IO
import System.IO.Error (isDoesNotExistError)
import System.Random (uniformR, StdGen)

import Printer
import System.Timeout (timeout)

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

-- [[[ Data generation ]]]

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

-- [[[ Utility functions ]]]

vsort :: Vector Int -> Vector Int
vsort v' = V.create do
  v <- V.unsafeThaw v'
  VA.sort v
  pure v

vinverse :: Vector Int -> Vector Int
vinverse p = V.update_ p p (V.enumFromN 0 $ V.length p)

-- [[[ Orphan instances ]]]

instance Default (V.Vector a) where
  def = V.empty

instance Default (VS.Vector a) where
  def = VS.empty
