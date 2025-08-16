{-# LANGUAGE NoFieldSelectors #-}
{-# OPTIONS_GHC -Wno-unused-do-bind #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}

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
  Solution(..),
  hs,
  hsio,
  cpp,

  Tests(..),
  testset,
  subtask,
  seedTests,
  allTests,

  runSolutions,
  generateTests',

  module Cpmonad.Gen,
  module Cpmonad.Printer,
  module Cpmonad.Misc,

  Default(..),
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
import Data.Set qualified as Set
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import GHC.Generics (Generic)
import System.CPUTime
import System.Directory
import System.Exit (ExitCode(..))
import System.IO
import System.IO.Error (isDoesNotExistError)
import System.IO.Temp (withTempFile)
import System.Timeout (timeout)
import System.Process

import Cpmonad.Gen
import Cpmonad.Printer
import Cpmonad.Misc
import System.Random (StdGen, mkStdGen)
import Data.Semigroup

-- Pts x where x must be between 0 and 1
data Verdict = Pts Float | Bad VerdictBad deriving (Show)

data VerdictBad = PE | RE String | TLE | Other String deriving (Show)

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
    { tests :: Tests (i, a),
      sols :: [Solution i o],
      check :: i -> a -> o -> Bool,
      printerI :: Printer i,
      printerA :: Printer (i, a),
      printerO :: Printer (i, o),
      timeLimit :: Int
    } ->
    Problem i a o

-- important: doesn't not check for name collisions

data Tests a = Tests
  { testsets :: Map String [a],
    subtaskIncludes :: Map String [String],  -- refers to testsets
    subtasks :: [String]
  }
  deriving (Functor, Generic, NFData)

-- Getting out of ST because I don't want to deal with impredicative types
type UnseededTests a = Tests (Gen a)

instance Show (Tests a) where
  show Tests {..} =
    "Tests {"
      <> "testsets = " <> show (([] :: [()]) <$ testsets) <> ", "
      <> "subtaskIncludes = " <> show subtaskIncludes <> ", "
      <> "subtasks = " <> show subtasks
      <> "}"

instance Semigroup (Tests a) where
  a <> b =
    Tests
      { testsets = Map.unionWith (<>) a.testsets b.testsets,
        subtaskIncludes = Map.unionWith (<>) a.subtaskIncludes b.subtaskIncludes,
        subtasks = a.subtasks <> b.subtasks
      }

instance Monoid (Tests a) where
  mempty = Tests mempty mempty mempty

testset :: String -> [Gen a] -> UnseededTests a
testset name gens = mempty { testsets = Map.singleton name gens }

subtask :: String -> [String] -> [Gen a] -> UnseededTests a
subtask name includes extra =
  mempty
    { subtaskIncludes = Map.singleton name includes,
      subtasks = singleton name
    }
    <> case extra of
      [] -> mempty
      _ ->
        mempty
          { testsets = Map.singleton ("subtask-" <> name) extra,
            subtaskIncludes = Map.singleton name ["subtask-" <> name]
          }

seedTests :: StdGen -> UnseededTests a -> Tests a
seedTests s tests = tests { testsets = fst (runGen everything s) }
  where
    everything = traverse sequenceA tests.testsets

allTests :: Tests a -> [(String, a)]
allTests tests = do
  let allTestsets = unique $ concatMap (tests.subtaskIncludes Map.!) tests.subtasks
  (seti, setname) <- zip [0 ..] allTestsets
  (testi, test) <- zip [0 ..] $ tests.testsets Map.! setname
  pure (show seti <> "-" <> setname <> "-" <> show testi, test)
  where
    unique xs = reverse . snd $ foldl' (\(s, r) x -> if Set.member x s then (s, r) else (Set.insert x s, x : r)) (mempty, mempty) xs

data Solution i o
  = SolutionHs {name :: String, f :: i -> IO o}
  | SolutionExt
      { name :: String,
        compileCmds :: [(FilePath, [String])],
        runCmd :: (FilePath, [String]),
        cleanupCmds :: [(FilePath, [String])]
      }

hs :: String -> (i -> o) -> Solution i o
hs name f = SolutionHs {name, f = pure . f}

hsio :: String -> (i -> IO o) -> Solution i o
hsio name f = SolutionHs {..}

cpp :: String -> Solution i o
cpp name =
  SolutionExt
    { name,
      compileCmds = [("g++", ["./" <> name <> ".cpp", "-o", "tmp/" <> name <> ".exe", "-O3", "-march=native", "-Wall"])],
      runCmd = ("tmp/" <> name <> ".exe", []),
      cleanupCmds = []
    }

wrapAction :: String -> IO a -> IO a
wrapAction name step = do
  putStrLn $ ":: " <> name <> " ..."
  start <- getCPUTime
  res <- step
  end <- getCPUTime
  let diffms = (round :: Double -> Int) $ fromIntegral (end - start) / (10 ^ (9 :: Int))
  putStrLn $ "   took " <> show diffms <> "ms"
  pure res

cleanDirectory :: FilePath -> IO ()
cleanDirectory dir = do
    catchJust (\e -> if isDoesNotExistError e then Just e else Nothing)
      (removeDirectoryRecursive dir)
      (const $ pure ())
    createDirectoryIfMissing False dir

generateTests' :: Problem i a o -> IO ()
generateTests' Problem {..} = do
  wrapAction "cleaning up" $ cleanDirectory "tests"

  _ <- wrapAction "evaluating tests" $ evaluate $ force tests


  wrapAction "outputting tests" do
    forM_ (allTests tests) \(testName, (i, a)) -> do
      h <- openBinaryFile ("tests/" <> testName <> ".in") WriteMode
      hSetBuffering h (BlockBuffering $ Just 4096)
      B.hPutBuilder h . fromJust $ printerI.toPrinted i
      hClose h

      h <- openBinaryFile ("tests/" <> testName <> ".out") WriteMode
      hSetBuffering h (BlockBuffering $ Just 4096)
      B.hPutBuilder h . fromJust $ printerA.toPrinted (i, a)
      hClose h

  wrapAction "parsing outputs" do
    forM_ (allTests tests) \(testName, _) -> do
      -- TODO
      s <- B.readFile ("tests/" <> testName <> ".in")
      let i = fst . fromJust $ printerI.fromPrinted (def, s)
      s <- B.readFile ("tests/" <> testName <> ".out")
      let a = fst <$> printerA.fromPrinted ((i, def), s)
      _ <- evaluate $ force (i, a)
      pure ()

  -- only for checking the performance of the printer itself
  wrapAction "transcoding tests" do
    forM_ (allTests tests) \(_, (i, a)) -> do
      let i' = fmap fst . printerI.fromPrinted . (def,) . B.toStrict . B.toLazyByteString . fromJust $ printerI.toPrinted i
      let a' = do i <- i'
                  fmap (snd . fst) . printerA.fromPrinted . ((i, def),) . B.toStrict . B.toLazyByteString . fromJust $ printerA.toPrinted (i, a)
      when (i' /= Just i) $ putStrLn "input didn't match!!!"
      when (a' /= Just a) $ putStrLn "aux didn't match!!!"

runSolutions :: Problem i a o -> IO ()
runSolutions Problem {..} = do
  -- TODO: parallelize. requires reworking
  -- TODO: fix the time, it shows "took 0ms"
  sols <- wrapAction "compiling" do
    cleanDirectory "tmp"
    flip filterM sols \case
      SolutionHs {} -> pure True
      SolutionExt {name, compileCmds} -> do
        putStrLn $ "-- " <> name
        handle @IOError (const $ putStrLn "-- compilation failed" >> pure False) do
          forM_ compileCmds (uncurry callProcess)
          pure True

  verdicts <- wrapAction "running" do
    forM sols \sol -> do
      putStr $ "sol " <> sol.name <> ": "
      verdict <-
        foldl1' mergeVerdict' <$> forM (allTests tests) \(testName, (i, a)) -> do
          (o' :: Either VerdictBad o) <- case sol of

            SolutionHs {f} ->
              handle @SomeException (pure . Left . RE . show) $
                maybe (Left TLE) Right <$> timeout timeLimit (evaluate . force =<< f i)

            SolutionExt {name, runCmd = (cmd, args)} -> do
              withTempFile "tmp/" (testName <> ".out") \outPath hOut -> do
                code <- withFile ("tests/" <> testName <> ".in") ReadMode \hIn -> do
                  let timeoutString = show (fromIntegral (round (fromIntegral timeLimit / 1000)) / 1000) <> "s"
                  withCreateProcess
                    -- GNU coreutils https://www.gnu.org/software/coreutils/timeout
                    (proc "timeout" $ ["--signal=KILL", timeoutString, cmd] <> args)
                      { delegate_ctlc = False,
                        std_in = UseHandle hIn,
                        -- hOut is closed here
                        std_out = UseHandle hOut
                      }
                    \_ _ _ p -> waitForProcess p
                case code of
                  -- timeout
                  ExitFailure 124 -> do
                    pure $ Left TLE
                  ExitFailure 125 -> do
                    pure $ Left $ RE "timeout command returned 125"
                  ExitFailure code -> do
                    pure $ Left $ RE $ "exit code " <> show code
                  ExitSuccess -> do
                    output <- B.readFile outPath
                    pure $ case printerO.fromPrinted ((i, def), output) of
                      Nothing -> Left PE
                      Just ((_, o), _) -> Right o
          case o' of
            Left e -> do
              putStr (case e of TLE -> "T"; RE _ -> "R"; PE -> "P"; Other _ -> "O")
              pure (Bad e, (i, a, Nothing))
            Right o ->
              if check i a o
                then putStr "." >> pure (ac, (i, a, Just o))
                else putStr "X" >> pure (wa, (i, a, Just o))
      putStrLn ""
      pure verdict

  wrapAction "evaluating" do
    forM_ (zip sols verdicts) \case
      (sol, (Pts x, _)) | x > 0 -> do
        putStrLn $ "sol " <> sol.name <> ": Pts " <> show (round (x * 100) :: Int) <> "%"
      (sol, (verdict, (i, a, o))) -> do
        putStrLn $ "sol " <> sol.name <> ": " <> case verdict of { Pts _ -> "WA"; Bad x -> show x } <> ":"
        B.putStrLn $ ">>> input:\n" <> (B.take 100 . B.toStrict . B.toLazyByteString . fromJust $ printerI.toPrinted i) <> "\n"
        case o of
          Just o -> B.putStrLn $ ">>> output:\n" <> (B.take 100 . B.toStrict . B.toLazyByteString . fromJust $ printerO.toPrinted (i, o)) <> "\n"
          _ -> pure ()
        B.putStrLn $ ">>> judge output:\n" <> (B.take 100 . B.toStrict . B.toLazyByteString . fromJust $ printerA.toPrinted (i, a)) <> "\n"
