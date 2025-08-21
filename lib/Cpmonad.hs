{-# LANGUAGE NoFieldSelectors #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
{-# OPTIONS_GHC -Wno-unused-do-bind #-}

{- |
Module     : Cpmonad
Description: Competitive programming problemsetting toolchain
Copyright  : (c) drdilyor, 2025
License    : MIT
Stability  : experimental
Portability: GNU/Linux

Cpmonad (a typo of Comonad) is a set of tools for setting competitive programming
problems. It features easy bidirectional parser/serializer, set of generators,
and tools to automatically run the solutions on all tests. There is no need to
write checkers for input and output formats.

This is very much an experiment. It only supports Batch problems, and doesn't
yet support exporting to polygon or other formats.
-}
module Cpmonad (
  -- * Problem
  Problem (..),
  generateTests,
  runSolutions,

  -- * Solution
  Solution (..),
  hs,
  hsio,
  cpp,

  -- * Tests
  Tests (..),
  UnseededTests,
  testset,
  subtask,
  seedTests,
  allTests,

  -- * Verdict
  Verdict (..),
  VerdictBad (..),
  wa,
  ac,
  mkPts,
  getPoints,
  hasPoints,
  mergeVerdict',

  -- * Other modules
  module Cpmonad.Gen,
  module Cpmonad.Printer,
  module Cpmonad.Misc,
) where

import Control.DeepSeq
import Control.Exception
import Control.Monad
import Data.ByteString.Builder qualified as B
import Data.ByteString.Char8 qualified as B
import Data.Default
import Data.List
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (fromJust)
import Data.Set qualified as Set
import Data.Vector qualified as V
import Data.Vector.Mutable qualified as VM
import GHC.Generics (Generic)
import System.CPUTime
import System.Directory
import System.Exit (ExitCode (..))
import System.IO
import System.IO.Error (isDoesNotExistError)
import System.IO.Temp (withTempFile)
import System.Process
import System.Timeout (timeout)

import Control.Applicative ((<|>))
import Control.Concurrent
import Cpmonad.Gen
import Cpmonad.Misc
import Cpmonad.Printer
import Data.IORef
import System.Random (StdGen)

{- | Central data specifying every part of the problem/task.

It has three type parameters:

- @i@ is passed to solutions
- @a@ is extra information stored with tests and passed onto grader.
- @o@ is what solutions need to compute

@i@ is written out as .in files, @a@ as .out files

A problem requires 3 printers for each of the type parameters.
@printerA@/@printerO@ accepts @i@ to support parsing outputs that depend on input.

Currently it only supports batch tasks.
-}
data Problem i a o where
  Problem
    :: (NFData i, NFData a, NFData o, Default i, Default a, Default o, Eq i, Eq a, Eq o)
    => { tests :: Tests (i, a)
       , sols :: [Solution i o]
       , check :: i -> a -> o -> Bool
       , printerI :: Printer i
       , printerA :: Printer (i, a)
       , printerO :: Printer (i, o)
       , timeLimit :: Int
        -- ^ time limit in microseconds
       }
    -> Problem i a o

-- | Generate all tests of the problem to a directory @tests@. Cleans the directory in the process
generateTests
  :: Int
  -- ^ number of (green)threads to use. Set it to @corecount - 1@ at most
  -> Problem i a o
  -> IO ()
generateTests threads Problem{..} = do
  wrapAction "cleaning up" $ cleanDirectory "tests"

  _ <- wrapAction "evaluating tests" $ do
    parallelPooled threads progressCounter $ map (evaluate . force) $ concat $ Map.elems tests.testsets
    putStrLn ""

  wrapAction "outputting tests" do
    parallelPooled threads progressCounter $ flip map (allTests tests) \(testName, (i, a)) -> do
      h <- openBinaryFile ("tests/" <> testName <> ".in") WriteMode
      hSetBuffering h (BlockBuffering $ Just 4096)
      B.hPutBuilder h . fromJust $ printerI.toPrinted i
      hClose h

      h <- openBinaryFile ("tests/" <> testName <> ".out") WriteMode
      hSetBuffering h (BlockBuffering $ Just 4096)
      B.hPutBuilder h . fromJust $ printerA.toPrinted (i, a)
      hClose h
    putStrLn ""

  wrapAction "parsing outputs" do
    parallelPooled threads progressCounter $ flip map (allTests tests) \(testName, (i, a)) -> do
      s <- B.readFile ("tests/" <> testName <> ".in")
      i' <- evaluate . force $ printerI.fromPrinted (def, s)
      case i' of
        Nothing -> throwIO $ AssertionFailed $ "tests/" <> testName <> ".in: failed to parse input"
        Just (i', _) | i' /= i -> throwIO $ AssertionFailed $ "tests/" <> testName <> ".in: parsed input is different from original"
        Just (i', _) -> do
          s <- B.readFile ("tests/" <> testName <> ".out")
          a' <- evaluate . force $ printerA.fromPrinted ((i', def), s)
          case a' of
            Nothing -> throwIO $ AssertionFailed $ "tests/" <> testName <> ".in: failed to parse input"
            Just ((_, a'), _) | a' /= a -> throwIO $ AssertionFailed $ "tests/" <> testName <> ".in: parsed input is different from original"
            Just _ -> pure ()
    putStrLn ""

  -- TODO: move this garbage out of here
  let debug = False
  when debug do
    -- only for checking the performance of the printer itself
    wrapAction "transcoding tests" do
      forM_ (allTests tests) \(_, (i, a)) -> do
        let i' = fmap fst . printerI.fromPrinted . (def,) . B.toStrict . B.toLazyByteString . fromJust $ printerI.toPrinted i
        let a' = i' >>= \i -> fmap (snd . fst) . printerA.fromPrinted . ((i, def),) . B.toStrict . B.toLazyByteString . fromJust $ printerA.toPrinted (i, a)
        i' `deepseq` a' `deepseq` pure ()

{- | Run all solutions on all tests.
It is required to call 'generateTests' before this unless the tests are up to date
-}
runSolutions
  :: Int
  -- ^ number of (green)threads to use. Set it to @corecount - 1@ at most
  -> Problem i a o
  -> IO ()
runSolutions threads Problem{..} = do
  sols <- wrapAction "compiling" do
    cleanDirectory "tmp"
    filterM compileSolution sols

  verdicts <- wrapAction "running" do
    forM sols \sol -> do
      let onProgress xs = do
            putStr ("\r" <> "sol " <> sol.name <> ": " <> status)
            hFlush stdout
           where
            status = flip map (V.toList xs) $ \case
              Nothing -> ' '
              Just (Bad e, _) -> case e of TLE -> 'T'; RE _ -> 'R'; PE -> 'P'; Other _ -> 'O'
              Just (pts, _) -> if hasPoints pts then '.' else 'X'

      results <- parallelPooled threads onProgress $ flip map (allTests tests) \(testName, (i, a)) -> do
        o <- runSolutionOnTest testName i sol
        evaluateOutput testName i a o
      putStrLn ""
      pure $ foldl1' mergeVerdict' results

  wrapAction "evaluating" do
    forM_ (zip sols verdicts) \case
      (sol, (Pts x, _)) | x > 0 -> do
        putStrLn $ "sol " <> sol.name <> ": Pts " <> show (round (x * 100) :: Int) <> "%"
      (sol, (verdict, (testName, i, _, o))) -> do
        let readFileHead f = withFile f ReadMode $ flip B.hGetSome 100
        putStrLn $ "sol " <> sol.name <> ": " <> case verdict of { Pts _ -> "WA"; Bad x -> show x } <> ":"

        input <- readFileHead ("tests/" <> testName <> ".in")
        B.putStrLn $ ">>> input:\n" <> input <> "\n"
        judgeOutput <- readFileHead ("tests/" <> testName <> ".out")
        B.putStrLn $ ">>> judge output:\n" <> judgeOutput <> "\n"
        case o of
          Just o -> B.putStrLn $ ">>> output:\n" <> (B.take 100 . B.toStrict . B.toLazyByteString . fromJust $ printerO.toPrinted (i, o)) <> "\n"
          _ -> pure ()
 where
  compileSolution = \case
    SolutionHs{} -> pure True
    SolutionExt{name, compileCmds} -> do
      putStrLn $ "-- " <> name
      handle @IOError (const $ putStrLn "-- compilation failed" >> pure False) do
        forM_ compileCmds (uncurry callProcess)
        pure True

  runSolutionOnTest testName i = \case
    SolutionHs{f} ->
      handle @SomeException (pure . Left . RE . show) $
        maybe (Left TLE) Right <$> timeout timeLimit (evaluate . force =<< f i)
    SolutionExt{runCmd = (cmd, args)} -> do
      withTempFile "tmp/" (testName <> ".out") \outPath hOut -> do
        code <- withFile ("tests/" <> testName <> ".in") ReadMode \hIn -> do
          let timeoutString = show (fromIntegral (round (fromIntegral timeLimit / 1000)) / 1000) <> "s"
          withCreateProcess
            -- GNU coreutils https://www.gnu.org/software/coreutils/timeout
            (proc "timeout" $ ["--signal=KILL", timeoutString, cmd] <> args)
              { delegate_ctlc = False
              , std_in = UseHandle hIn
              , -- hOut is closed here
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

  evaluateOutput testName i a o =
    pure $ case o of
      Left e -> (Bad e, (testName, i, a, Nothing))
      Right o
        | check i a o -> (ac, (testName, i, a, Just o))
        | otherwise -> (wa, (testName, i, a, Just o))

wrapAction :: String -> IO a -> IO a
wrapAction name step = do
  -- TODO: fix the time, use wall time
  putStrLn $ ":: " <> name <> " ..."
  start <- getCPUTime
  res <- step
  end <- getCPUTime
  let diffms = (round :: Double -> Int) $ fromIntegral (end - start) / (10 ^ (9 :: Int))
  putStrLn $ "   took " <> show diffms <> "ms"
  pure res

cleanDirectory :: FilePath -> IO ()
cleanDirectory dir = do
  catchJust
    (\e -> if isDoesNotExistError e then Just e else Nothing)
    (removeDirectoryRecursive dir)
    (const $ pure ())
  createDirectoryIfMissing False dir

-- we use explicit semaphores because otherwise multiple greenthreads will be interleaved on a single core,
-- which will lead to TLEs because `timeout` considers total wall time
parallelPooled :: Int -> (V.Vector (Maybe a) -> IO ()) -> [IO a] -> IO [a]
parallelPooled n onProgress actions = do
  sem <- newQSem n
  results <- VM.replicate (length actions) Nothing
  hasProgress <- newEmptyMVar
  anError <- newIORef Nothing
  channel <- newChan
  onProgress =<< V.freeze results

  let debug = False
  forM_ (zip [0 ..] actions) $ \(i, action) -> forkIO $ do
    result <- try @SomeException $ bracket_ (waitQSem sem) (signalQSem sem) $ do
      when debug do
        putStrLn $ ">> " <> show i
        hFlush stdout
      x <- action >>= evaluate
      when debug do
        putStrLn $ "-- " <> show i
        hFlush stdout
      pure x
    writeChan channel (i, result)

  let progressThread = do
        () <- takeMVar hasProgress
        onProgress =<< V.freeze results
        threadDelay 200_000
        progressThread

  bracket (forkIO progressThread) killThread \_ -> do
    replicateM_ (length actions) do
      (i, val) <- readChan channel
      case val of
        Left e -> modifyIORef' anError (<|> Just e)
        Right x -> do
          VM.write results i $ Just x
          void $ tryPutMVar hasProgress ()

    onProgress =<< V.freeze results
    readIORef anError >>= \case
      Just e -> throwIO e
      _ -> V.toList . V.map fromJust <$> V.freeze results

progressBar :: V.Vector (Maybe a) -> IO ()
progressBar v = do
  putStr $ "\r   " <> map (\case Just _ -> '.'; Nothing -> ' ') (V.toList v)
  hFlush stdout

progressCounter :: V.Vector (Maybe a) -> IO ()
progressCounter v = do
  putStr $ "\r   " <> show doneCount <> "/" <> show (V.length v)
  hFlush stdout
 where
  doneCount = V.sum $ V.map (\case Just _ -> 1; Nothing -> 0) v

{- | A solution can either be a Haskell function or a collection of commands.
The commands can use the @tmp@ directory in the current directory.

/The names must be unique./

'SolutionExt'\'s @runCmd@\'s output will be redirected to a file.
-}
data Solution i o
  = SolutionHs {name :: String, f :: i -> IO o}
  | SolutionExt
      { name :: String
      , compileCmds :: [(FilePath, [String])]
      , runCmd :: (FilePath, [String])
      , cleanupCmds :: [(FilePath, [String])]
      }

-- | Pure Haskell solution
hs :: String -> (i -> o) -> Solution i o
hs name f = SolutionHs{name, f = pure . f}

-- | IO Haskell solution
hsio :: String -> (i -> IO o) -> Solution i o
hsio name f = SolutionHs{..}

-- | C++ solution with the file @\<name\>.cpp@ in the current directory and flags @-O2 and -Wall@
cpp
  :: String
  -- ^ name of the solution
  -> Solution i o
cpp name = cpp' ["-O2", "-Wall"] (name <> ".cpp") name

-- | C++ solution with explicit path to the .cpp file and explicit compile-flags
cpp'
  :: [String]
  -- ^ @g++@ flags
  -> FilePath
  -- ^ path to the @ cpp@ file
  -> String
  -- ^ name of the solution
  -> Solution i o
cpp' flags path name =
  SolutionExt
    { name
    , compileCmds = [("g++", [path, "-o", "tmp/" <> name <> ".exe"] <> flags)]
    , runCmd = ("tmp/" <> name <> ".exe", [])
    , cleanupCmds = []
    }

{- | Collection of 'testset's and 'subtask's.
A testset is a named list of @a@. Testsets can be included in subtasks.

/Tests that are never referenced from subtasks are ignored./

Important: doesn't not check for name collisions. Everything is merged!

Note: the type parameter @a@ has nothing to do with @a@ in 'Problem'!

==== __Examples:__

@
testset "sample" [pure 1, pure 2]
<> testset "small" $ replicate 5 $ genr 1 100
<> testset "big"   $ replicate 5 $ genr 100 (10^9)
<> subtask "brute" ["sample", "small"] []
<> subtask "full" ["sample", "small", "big"] []
@
-}
data Tests a = Tests
  { testsets :: Map String [a]
  , subtaskIncludes :: Map String [String]
  -- ^ mapping from subtask names to testset names
  , subtasks :: [String]
  }
  deriving (Functor, Generic, NFData)

-- | Tests that need to be seeded yet
type UnseededTests a = Tests (Gen a)

instance Show (Tests a) where
  show Tests{..} =
    mconcat
      [ "Tests"
      , " {testsets = " <> show (([] :: [()]) <$ testsets)
      , ", subtaskIncludes = " <> show subtaskIncludes
      , ", subtasks = " <> show subtasks
      , "}"
      ]

instance Semigroup (Tests a) where
  a <> b =
    Tests
      { testsets = Map.unionWith (<>) a.testsets b.testsets
      , subtaskIncludes = Map.unionWith (<>) a.subtaskIncludes b.subtaskIncludes
      , subtasks = a.subtasks <> b.subtasks
      }

instance Monoid (Tests a) where
  mempty = Tests mempty mempty mempty

-- | Make a single testset with the given name and the objects
testset :: String -> [Gen a] -> UnseededTests a
testset name gens = mempty{testsets = Map.singleton name gens}

{- | Make a subtask with the given name, included testsets, and extra tests.
Extra tests are put after the included testcases
-}
subtask :: String -> [String] -> [Gen a] -> UnseededTests a
subtask name includes extra =
  mempty
    { subtaskIncludes = Map.singleton name includes
    , subtasks = singleton name
    }
    <> case extra of
      [] -> mempty
      _ ->
        mempty
          { testsets = Map.singleton ("subtask-" <> name) extra
          , subtaskIncludes = Map.singleton name ["subtask-" <> name]
          }

-- | Threads the StdGen through all the tests
seedTests :: StdGen -> UnseededTests a -> Tests a
seedTests s tests = tests{testsets = fst (runGen everything s)}
 where
  everything = traverse sequenceA tests.testsets

{- | The list of tests with their unique names.

/Tests that are never referenced from subtasks are ignored./

==== __Examples:__

>>> t1 = testset "a" [pure "a1", pure "a2"]
>>> t2 = testset "b" [pure "b1", pure "b2"]
>>> s = subtask "full" ["b", "a"] [pure "x"]
>>> allTests $ seedTests undefined $ t1 <> t2 <> s
[("0-b-0","b1"),("0-b-1","b2"),("1-a-0","a1"),("1-a-1","a2"),("2-subtask-full-0","x")]
-}
allTests :: Tests a -> [(String, a)]
allTests tests = do
  let allTestsets = unique $ concatMap (tests.subtaskIncludes Map.!) tests.subtasks
  (seti, setname) <- zip [0 ..] allTestsets
  (testi, test) <- zip [0 ..] $ tests.testsets Map.! setname
  pure (show seti <> "-" <> setname <> "-" <> show testi, test)
 where
  unique xs = reverse . snd $ foldl' (\(s, r) x -> if Set.member x s then (s, r) else (Set.insert x s, x : r)) (mempty, mempty) xs

-- | Verdict of a solution on a test
data Verdict
  = -- | the solution was graded successfully. Must be between 0 and 1
    Pts Float
  | -- | there was an error
    Bad VerdictBad
  deriving (Show, Eq)

-- | Verdict if there was an error running the solution
data VerdictBad
  = -- | output couldn't be parsed
    PE
  | -- | runtime error with extra information
    RE String
  | -- | time limit exceeded
    TLE
  | -- | any other message to the user
    Other String
  deriving (Show, Eq)

-- | Zero points. @Pts 0@
wa :: Verdict
wa = Pts 0

-- | Full points. @Pts 1@
ac :: Verdict
ac = Pts 1

-- | Constructs a 'Pts' by clamping the points between 0 and 1
mkPts :: Float -> Verdict
mkPts x = Pts (0 `max` x `min` 1)

-- | If 'Pts', gets the points, otherwise 0
getPoints :: Verdict -> Float
getPoints (Pts x) = x
getPoints _ = 0

-- | Whether it is 'Pts' and greater than zero
hasPoints :: Verdict -> Bool
hasPoints x = getPoints x > 0

-- | Return the argument which has smaller points. If an error, returns the left-most one
mergeVerdict' :: (Verdict, b) -> (Verdict, b) -> (Verdict, b)
mergeVerdict' a b = case (a, b) of
  ((Pts x, _), (Pts y, _))
    | x < y -> a
    | otherwise -> b
  ((Pts _, _), _) -> b
  (_, (Pts _, _)) -> a
  (_, _) -> a

-- | Semigroup based on 'mergeVerdict\''
instance Semigroup Verdict where
  a <> b = fst $ mergeVerdict' (a, ()) (b, ())

instance Monoid Verdict where
  mempty = Pts 1
