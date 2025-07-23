module Printer(
  Printer(..),
  readVecInt,
  readChar,
  readInt,
  nest,
  len,
) where

import Control.Monad.ST (runST)
import Data.ByteString.Builder qualified as B
import Data.ByteString.Char8 qualified as B
import Data.ByteString.Char8(ByteString)
import Data.Vector (Vector)
import Data.Vector qualified as V
import Data.Vector.Mutable qualified as VM
import Lens.Micro

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

len :: Lens' (Vector Int) Int
len = lens V.length (\_ n -> V.replicate n 0)
