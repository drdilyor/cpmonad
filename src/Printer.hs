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
  idl,
) where

import Control.Monad.ST (runST)
import Data.ByteString.Builder qualified as B
import Data.ByteString.Char8 qualified as B
import Data.ByteString.Char8(ByteString)
import Data.Vector (Vector, (!))
import Data.Vector qualified as V
import Data.Vector.Mutable qualified as VM

import Lens.Micro
import Data.Maybe (fromJust)

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


pvec :: Char -> SimpleGetter a Int -> Lens' a (Vector b) -> b -> Printer b -> Printer a
pvec sep _ _ _ _ | sep /= ' ' && sep /= '\n' = error "separator must be whitespace"
pvec sep n arr e (Printer toPrinted' fromPrinted') = Printer {..}
  where
    toPrinted !x
      | x ^. n /= V.length (x ^. arr) = Nothing
      | x ^. n == 0 = Just mempty
      | otherwise =
          toPrinted' (V.head (x ^. arr))
          <> mconcat [Just (B.char8 sep) <> toPrinted' el | el <- V.toList . V.tail $ x ^. arr]
    fromPrinted (!x, !s') =
      let count = x ^. n
       in if count == 0
            then Just (x & arr .~ V.empty, s')
            else runST do
              v <- VM.new count
              let go !s !i
                    | i == count = pure (Just s)
                    | otherwise =
                        case fromPrinted' (e, s)  of
                          Nothing -> pure Nothing
                          Just (val, s'') -> VM.write v i val >> go s'' (i + 1)
              res <- go s' 0
              v' <- V.freeze v
              pure $ (x & arr .~ v',) <$> res

pvecint :: Char -> SimpleGetter a Int -> Lens' a (V.Vector Int) -> Printer a
pvecint sep n arr = Printer {..}
  where
    toPrinted !x
      | x ^. n /= V.length (x ^. arr) = Nothing
      | x ^. n == 0 = Just mempty
      | otherwise = Just $ B.intDec (V.head (x ^. arr)) <> mconcat [B.char8 sep <> B.intDec el | el <- V.toList . V.tail $ x ^. arr]
    fromPrinted (!x, !s') =
      let count = x ^. n
       in if count == 0
            then Just (x & arr .~ V.empty, s')
            else runST do
              v <- VM.new count
              let go !s !i
                    | i == count = pure (Just s)
                    | otherwise =
                        case B.readInt (skipSpace s) of
                          Nothing -> pure Nothing
                          Just (num, s'') -> VM.write v i num >> go s'' (i + 1)
              res <- go s' 0
              v' <- V.freeze v
              pure $ (x & arr .~ v',) <$> res

pvecvecint :: SimpleGetter a Int -> SimpleGetter a Int -> Lens' a (Vector (Vector Int)) -> Printer a
pvecvecint n m arr = Printer {..}
  where
    toPrinted !x
      | x ^. n /= V.length (x ^. arr) = Nothing
      | otherwise =
            let vec i = (pvecint ' ' _1 _2).toPrinted (x ^. m, (x ^. arr) ! i)
             in mconcat [vec i <> Just (B.char8 '\n') | i <- [0 .. x ^. n - 1]]
    fromPrinted (!x, !s') =
      let vec s i = (pvecint ' ' _1 _2).fromPrinted ((x ^. m, (x ^. arr) ! i), s)
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


len :: Lens' (Vector Int) Int
len = lens V.length (\_ n -> V.replicate n 0)

idl :: Lens' a a
idl = lens id (const id)
