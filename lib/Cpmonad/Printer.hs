{-# OPTIONS_GHC -Wno-unused-imports #-}

-- | Combinators for defining a parser and a serializer with a single definition
module Cpmonad.Printer (
  Printer (..),
  char,
  sp,
  endl,
  pint,
  pvec,
  pvecN,
  pvecint,
  pvecintN,
  pvecvecint,
  nest,
  len,
) where

import Control.Monad.ST (runST)
import Cpmonad.Misc
import Data.ByteString.Builder qualified as B
import Data.ByteString.Char8 (ByteString)
import Data.ByteString.Char8 qualified as B
import Data.Default
import Data.Vector (Vector, (!))
import Data.Vector qualified as V
import Data.Vector.Mutable qualified as VM
import Lens.Micro

{- | A printer is both a parser and a serializer.

Parsing is done in an "FSM" fashion. Each parser gets the current state and
updates it with the new information. The state is initialized to 'def'.
To combine printers, the printers must operate on the same state type @a@.
Printers take lens to the part of the object to operate on. See the examples.

All of the builtin printers ignore extra whitespace.

__Important__: the printers here, on their own, are valid — parser is
an inverse of the serializer. But when combined, they can still become ambiguous.
To prevent that, you must use whitespace when combining parsers. If I were to
make 'pint' output space, it would lead to trailing spaces in lines. Therefore,
you need to choose between sp and endl explicit each time. 'generateTests'
will check for this and will complain if your printer is invalid.

The following characters are considered whitespace: @" \\r\\n"@

==== __Examples:__

Full-blown example:

@
data Input = Input {_k :: Int, _arr :: Vector Int, _queries :: Vector (Int, Int)}
makeLenses ''Input
printer = pint (arr . len) <> sp <> pint k <> endl
  <> pvecint sp arr <> endl
  <> pint (queries . len)
  <> pvec endl queries (pint _1 <> sp <> pint _2) <> endl
@

Example input:

@
4 25
1 1 2 1
3
0 1
0 2
1 3
@

Parsed object:

@
Input1
  { _k        = 25
  , _arr     = [1,1,2,1]
  , _queries = [(0,1),(0,2),(1,3)]
  }
@

Printer for a single integers:

>>> pint id :: Printer Int

Example input:

@
42
@

Printer for a pair of integers

>>> pint _1 <> sp <> pint _2 :: Printer (Int, Int)

Example input:

@
42 37
@

Printer for an array of integers:

>>> pint len <> pvecint sp id :: Printer (Vector Int)

Example input:

@
5
1 2 3 40 500
@

Printer for an array of integers with explicit field containing their count:

>>> pint _1 <> pvecN sp _1 _2 (pint id) :: Printer (Int, Vector Int)

Example input: the same as above
-}
data Printer a = Printer
  { toPrinted :: a -> Maybe B.Builder
  , fromPrinted :: (a, ByteString) -> Maybe (a, ByteString)
  }

instance Semigroup (Printer a) where
  p1 <> p2 = Printer{..}
   where
    toPrinted xb = p1.toPrinted xb <> p2.toPrinted xb
    fromPrinted xs = p1.fromPrinted xs >>= p2.fromPrinted

skipSpace :: ByteString -> ByteString
skipSpace = B.dropWhile (\c -> c == ' ' || c == '\n' || c == '\r') -- idc about other ascii ws chars

-- | Outputs the character, and consumes character after skipping whitespace. /Don't use this for whitespace/
char :: Char -> Printer a
char c = Printer{..}
 where
  toPrinted !_ = Just $ B.char8 c
  fromPrinted (!x, !s) = do
    c' <- skipSpace s `B.indexMaybe` 0
    if c' == c then Just (x, B.tail s) else Nothing

-- | Outputs a single space, and consumes all whitespace. Doesn't change the state.
sp :: Printer a
sp = Printer{..}
 where
  toPrinted !_ = Just $ B.char8 ' '
  fromPrinted (!x, !s) = Just (x, skipSpace s)

-- | Outputs a newline (LF), and consumes all whitespace. Doesn't change the state.
endl :: Printer a
endl = Printer{..}
 where
  toPrinted !_ = Just $ B.char8 '\n'
  fromPrinted (!x, !s) = Just (x, skipSpace s)

-- | Focuses the printer on part of the state. See the implementation if confused.
nest :: Printer b -> Lens' a b -> Printer a
nest p f = Printer{..}
 where
  toPrinted !a = p.toPrinted (a ^. f)
  fromPrinted (!a, !s) =
    let x = p.fromPrinted (a ^. f, s)
     in case x of
          Just (b, c) -> Just (a & f .~ b, c)
          Nothing -> Nothing

-- extremely scuffed but trust me i couldn't find a better way
pint :: Lens' a Int -> Printer a
pint = nest Printer{..}
 where
  toPrinted !x = Just $ B.intDec x
  fromPrinted (!_, !s) = do
    (n, s') <- B.readInt (skipSpace s)
    pure (n, s')

pvec
  :: (Default b)
  => (forall c. Printer c)
  -- ^ separator (e.g. 'sp')
  -> Lens' a (Vector b)
  -> Printer b
  -> Printer a
pvec sep arr p = pvecN sep (arr . len) arr p

pvecN
  :: (Default b)
  => (forall c. Printer c)
  -- ^ separator
  -> SimpleGetter a Int
  -- ^ number of elements
  -> Lens' a (Vector b)
  -- ^ the vector itself
  -> Printer b
  -> Printer a
pvecN sep n arr p = Printer{..}
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
                          case p.fromPrinted (def, s2) of
                            Nothing -> pure Nothing
                            Just (val, s3) -> VM.write v i val >> go s3 (i + 1)
            res <- go s' 0
            v' <- V.freeze v
            pure $ (x & arr .~ v',) <$> res

pvecint :: (forall b. Printer b) -> Lens' a (V.Vector Int) -> Printer a
pvecint sep arr = pvec sep arr (pint id)

pvecintN :: (forall b. Printer b) -> SimpleGetter a Int -> Lens' a (V.Vector Int) -> Printer a
pvecintN sep n arr = pvecN sep n arr (pint id)

{- | This is a special parser for a matrix of integers. Each row is separated by newlines and
each number in the row is separated by spaces.
-}
pvecvecint :: SimpleGetter a Int -> SimpleGetter a Int -> Lens' a (Vector (Vector Int)) -> Printer a
pvecvecint n m arr = Printer{..}
 where
  toPrinted !x
    | x ^. n /= V.length (x ^. arr) = Nothing
    | otherwise =
        let vec i = (pvecintN sp _1 _2).toPrinted (x ^. m, (x ^. arr) ! i)
         in mconcat [vec i <> Just (B.char8 '\n') | i <- [0 .. x ^. n - 1]]
  fromPrinted (!x, !s') =
    let vec s i = (pvecintN sp _1 _2).fromPrinted ((x ^. m, (x ^. arr) ! i), s)
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

-- | Iso between Vector and its length. When set, it fills the vector with n @def@\'s.
len :: (Default a) => Lens' (Vector a) Int
len = lens V.length (\_ n -> V.replicate n def)
