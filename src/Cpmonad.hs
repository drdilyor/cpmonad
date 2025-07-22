{-# LANGUAGE NoFieldSelectors #-}

module Cpmonad(
  Verdict(..),
  mergeVerdict',
  Problem(..),

  Gen(..),
  runGen,
  genr,
  genri,
  Indexable(..),
  choose,

  Printer(..),
  readVecInt,
  readChar,
  readInt,
  nest,
) where

import Control.Monad.ST
import Data.ByteString.Builder qualified as B
import Data.ByteString.Char8 qualified as B
import Data.ByteString.Char8(ByteString)
import Data.List
import Data.Vector (Vector)
import Data.Vector qualified as V
import Data.Vector.Mutable qualified as VM
import Data.Vector.Strict qualified
import Lens.Micro
import System.Random (uniformR, StdGen)

-- [[[ Basics ]]]
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
    printerA :: Printer a,
    ei :: i, eo :: o, ea :: a
  }

-- [[[ Data generation ]]]

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

-- [[[ Parsing and validation ]]]

data Printer a = Printer
  { toPrinted :: (a, B.Builder) -> B.Builder,
    fromPrinted :: (a, ByteString) -> Maybe (a, ByteString)
  }

instance Semigroup (Printer a) where
  p1 <> p2 = Printer {..}
    where
      toPrinted xb = p1.toPrinted xb <> p2.toPrinted xb
      fromPrinted xs = p1.fromPrinted xs >>= p2.fromPrinted


readInt :: Printer Int
readInt = Printer {..}
  where
    toPrinted (!x, !b) = b <> B.intDec x
    fromPrinted (!_, !s) = do
      (n, s') <- B.readInt s
      pure (n, s')

readChar :: Char -> Printer a
readChar c = Printer {..}
  where
    toPrinted (!_, !b) = b <> B.char8 c
    fromPrinted (!x, !s) = do
      c' <- s `B.indexMaybe` 0
      if c' == c then Just (x, B.tail s) else Nothing

readVecInt :: Printer (Vector Int)
readVecInt = Printer {..}
  where
    toPrinted (!x, !b) =
      if V.length x == 0
        then b
        else
          V.foldl' (\a el -> a <> B.intDec el <> B.char8 ' ') b (V.init x)
            <> B.intDec (V.last x)
    fromPrinted (!x, !s') =
      let count = V.length x
      in if count == 0 then Just (V.empty, s') else runST do
        v <- VM.new (V.length x)
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
        pure $ (v',) <$> res

nest :: Lens' a b -> Printer b -> Printer a
nest f p = Printer {..}
  where
    toPrinted (!a, !builder) = p.toPrinted (a ^. f, builder)
    fromPrinted (!a, !s) =
      let x = p.fromPrinted (a ^. f, s)
       in case x of
            Just (b, c) -> Just (a & f .~ b, c)
            Nothing -> Nothing
