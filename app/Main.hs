{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Monoid law, left identity" #-}
module Main where

import Control.DeepSeq
import Data.Vector (Vector, (!))
import Data.Vector qualified as V
import Data.Default
import Data.ByteString.Char8 qualified as B
import Lens.Micro
import Lens.Micro.TH
import System.Random (mkStdGen)
import GHC.Generics

import Cpmonad
import Cpmonad.Misc
import Debug.Trace (traceShowId)

data Input = Input
  { _arr :: Vector Int,
    _queries :: Vector (Int, Int)
  }
  deriving (Show, Eq, Generic, NFData, Default)

type Output = Vector Int

makeLenses ''Input

tests :: Tests Input
tests =
  seedTests (mkStdGen 123) . traceShowId $
    mconcat
      [
        testset "manual" $ pure $ pure $
          Input
            (V.fromList [1,2,3])
            (V.fromList [(1,2), (0,2)]),
        testset "n3" $
          replicate 10 (gen1 200 200),
        testset "n2" $
          replicate 10 (gen2 2000 2000),
        testset "big" $
          replicate 20 (gen2 500_000 500_000),
        subtask "n3" ["manual", "n3"] [ gen1 400 100 ],
        subtask "n2" ["manual", "n3", "n2"] [],
        subtask "full" ["manual", "n3", "n2", "big"] []
      ]

threads = 12

p :: Problem Input Output Output
p =
  Problem
    { tests = fmap (\x -> (x, model x)) tests,
      sols =
        [ hs "sol11" sol1,
          hs "sol12" sol1,
          hs "sol13" sol1,
          hs "sol2" sol2,
          cpp "test"
        ],
      check = const (==),
      printerI = pint (arr . len) <> endl
                <> pvec sp arr (pint id) <> endl
                <> pint (queries . len) <> endl
                <> pvec endl queries (pint _1 <> sp <> pint _2),
      printerO = pvecintN sp (_1 . queries . len) _2,
      printerA = pvecintN endl (_1 . queries . len) _2,
      timeLimit = 1_000_000
    }


sol1 :: Input -> Output
sol1 (Input arr queries) =
  let pref = V.scanl' (+) 0 arr
      ans = flip V.map queries \(l, r) ->
        pref ! (r + 1) - pref ! l
   in ans

sol2 :: Input -> Output
sol2 (Input arr queries) =
  let ans = flip V.map queries \(l, r) ->
        V.sum (V.slice l (r - l + 1) arr)
   in ans

gen1 :: Int -> Int -> Gen Input
gen1 n q =
  Input
    <$> V.replicateM n (genr 0 (10^9))
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

main' :: IO ()
main' = generateTests threads p >> runSolutions threads p

data Input1 = Input1 {_k :: Int, _arr1 :: Vector Int, _queries1 :: Vector (Int, Int)}
  deriving (Show, Eq, Generic, NFData, Default)
makeLenses ''Input1
printer = pint (arr1 . len) <> sp <> pint k <> endl
  <> pvec sp arr1 (pint id) <> endl
  <> pint (queries1 . len)
  <> pvec endl queries1 (pint _1 <> sp <> pint _2) <> endl

main = print $ fst <$> (pint _1 <> pvecN sp _1 _2 (pint id) :: Printer (Int, Vector Int)).fromPrinted (def, "5 1 2 3 40 500")
  

