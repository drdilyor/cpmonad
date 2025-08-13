module Printer(
  Printer(..),
  char,
  pint,
  pvec,
  pvecint,
  nest,
  len,
  endl,
  sp,
  pvecvecint,
) where

import Control.Monad.ST (runST)
import Data.ByteString.Builder qualified as B
import Data.ByteString.Char8 qualified as B
import Data.ByteString.Char8(ByteString)
import Data.Default
import Data.Vector (Vector, (!))
import Data.Vector qualified as V
import Data.Vector.Mutable qualified as VM

import Lens.Micro

data Printer a = Printer
  { toPrinted :: a -> Maybe B.Builder,
    fromPrinted :: (a, ByteString) -> Maybe (a, ByteString)
  }

instance Semigroup (Printer a) where
  p1 <> p2 = Printer {..}
    where
      toPrinted xb = p1.toPrinted xb <> p2.toPrinted xb
      fromPrinted xs = p1.fromPrinted xs >>= p2.fromPrinted


skipSpace :: ByteString -> ByteString
skipSpace = B.dropWhile (\c -> c == ' ' || c == '\n')  -- windows sucks, so do other ascii ws chars

char :: Char -> Printer a
char c = Printer {..}
  where
    toPrinted !_ = Just $ B.char8 c
    fromPrinted (!x, !s) = do
      c' <- skipSpace s `B.indexMaybe` 0
      if c' == c then Just (x, B.tail s) else Nothing

sp :: Printer a
sp = Printer {..}
  where
    toPrinted !_ = Just $ B.char8 ' '
    fromPrinted (!x, !s) = Just (x, skipSpace s)

endl :: Printer a
endl = Printer {..}
  where
    toPrinted !_ = Just $ B.char8 '\n'
    fromPrinted (!x, !s) = Just (x, skipSpace s)

nest :: Printer b -> Lens' a b -> Printer a
nest p f  = Printer {..}
  where
    toPrinted !a = p.toPrinted (a ^. f)
    fromPrinted (!a, !s) =
      let x = p.fromPrinted (a ^. f, s)
       in case x of
            Just (b, c) -> Just (a & f .~ b, c)
            Nothing -> Nothing

-- extremely scuffed but trust me i couldn't find a better way
pint :: Lens' a Int -> Printer a
pint = nest Printer {..}
  where
    toPrinted !x = Just $ B.intDec x
    fromPrinted (!_, !s) = do
      (n, s') <- B.readInt (skipSpace s)
      pure (n, s')


pvec :: (forall c. Printer c) -> SimpleGetter a Int -> Lens' a (Vector b) -> b -> Printer b -> Printer a
pvec sep n arr e p = Printer {..}
  where
    toPrinted !x
      | x ^. n /= V.length (x ^. arr) = Nothing
      | x ^. n == 0 = Just mempty
      | otherwise =
          p.toPrinted (V.head (x ^. arr))
          <> mconcat [sep.toPrinted () <> p.toPrinted el | el <- V.toList . V.tail $ x ^. arr]
    fromPrinted (!x, !s') =
      let count = x ^. n
       in if count == 0
            then Just (x & arr .~ V.empty, s')
            else runST do
              v <- VM.new count
              let go !s !i
                    | i == count = pure (Just s)
                    | otherwise =
                        case sep.fromPrinted ((), s) of
                          Nothing -> pure Nothing
                          Just (_, s2) ->
                            case p.fromPrinted (e, s2)  of
                              Nothing -> pure Nothing
                              Just (val, s3) -> VM.write v i val >> go s3 (i + 1)
              res <- go s' 0
              v' <- V.freeze v
              pure $ (x & arr .~ v',) <$> res

pvecint :: (forall b. Printer b) -> SimpleGetter a Int -> Lens' a (V.Vector Int) -> Printer a
pvecint sep n arr = pvec sep n arr 0 (pint id)

pvecvecint :: SimpleGetter a Int -> SimpleGetter a Int -> Lens' a (Vector (Vector Int)) -> Printer a
pvecvecint n m arr = Printer {..}
  where
    toPrinted !x
      | x ^. n /= V.length (x ^. arr) = Nothing
      | otherwise =
            let vec i = (pvecint sp _1 _2).toPrinted (x ^. m, (x ^. arr) ! i)
             in mconcat [vec i <> Just (B.char8 '\n') | i <- [0 .. x ^. n - 1]]
    fromPrinted (!x, !s') =
      let vec s i = (pvecint sp _1 _2).fromPrinted ((x ^. m, (x ^. arr) ! i), s)
       in runST do
            let count = x ^. n
            v <- VM.new count
            let go !s !i
                  | i == count = pure (Just s)
                  | otherwise =
                      case vec s i of
                        Nothing -> pure Nothing
                        Just ((_, vv), s'') -> VM.write v i vv >> go s'' (i + 1)
            res <- go s' 0
            v' <- V.freeze v
            pure $ (x & arr .~ v',) <$> res


len :: Default a => Lens' (Vector a) Int
len = lens V.length (\_ n -> V.replicate n def)
