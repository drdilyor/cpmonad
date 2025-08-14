{-# OPTIONS_GHC -Wno-type-defaults #-}
{-# LANGUAGE TemplateHaskell #-}
module Main where

import Data.Vector (Vector, (!))
import Data.Vector qualified as V
import Lens.Micro
import Lens.Micro.TH
import System.Random (mkStdGen)

import Cpmonad

data Input = Input
  { _n :: Int,
    _arr :: Vector Int,
    _q :: Int,
    _queries :: Vector (Int, Int)
  }
  deriving (Show, Eq, Generic, NFData, Default)

type Output = Vector Int

makeLenses ''Input

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
      sols =
        [ hs "sol11" sol1,
          hs "sol12" sol1,
          hs "sol13" sol1,
          hs "sol2" sol2,
          cpp "test"
        ],
      check = const (==),
      printerI = pint n <> endl
                <> pvec sp n arr 0 (pint id) <> endl
                <> pint q <> endl
                <> pvec endl q queries (0,0) (pint _1 <> sp <> pint _2),
      printerO = pvecint sp (_1 . q) _2,
      printerA = pvecint endl (_1 . q) _2,
      timeLimit = 100_000
    }


sol1 :: Input -> Output
sol1 (Input _ arr _ queries) =
  let pref = V.scanl' (+) 0 arr
      ans = flip V.map queries \(l, r) ->
        pref ! (r + 1) - pref ! l
   in ans

sol2 :: Input -> Output
sol2 (Input _ arr _ queries) =
  let ans = flip V.map queries \(l, r) ->
        V.sum (V.slice l (r - l + 1) arr)
   in ans

gen1 :: Int -> Int -> Gen s Input
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

gen2 :: Int -> Int -> Gen s Input
gen2 = gen1

model :: Input -> Output
model = sol1

genAll :: forall a. (forall s. [Gen s a]) -> [a]
genAll xs = fst $ runGen (sequence xs) (mkStdGen 1)

main :: IO ()
main = generateTests' p >> runSolutions p
