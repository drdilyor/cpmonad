{-# OPTIONS_GHC -Wno-missing-signatures #-}
module Cpmonad.PrinterSpec where

import Prelude hiding (print)
import Data.ByteString.Builder qualified as B
import Data.ByteString.Char8 qualified as B
import Data.ByteString.Char8(ByteString)
import Data.Default
import Data.List (intersperse)
import Data.String (IsString)
import Data.Vector qualified as V
import Lens.Micro
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck

import Cpmonad.Printer

toPrinted' :: Printer a -> a -> Maybe ByteString
toPrinted' p x = B.toStrict . B.toLazyByteString <$> p.toPrinted x

sepBySp :: IsString s => [s] -> Gen [s]
sepBySp = sequence . intersperse (elements [" ", "  ", "   "]) . map pure

spec = do
  describe "pint" do
    let print = toPrinted' (pint id)
    let parse s = (pint id).fromPrinted (0, s)

    prop "fromPrinted is inverse of toPrinted" $
      \x -> (print x >>= parse) `shouldBe` Just (x, "")
    it "consumes whitespace correctly" do
      parse " 42  " `shouldBe` Just (42, "  ")

  describe "pvecint" do
    let print n v = toPrinted' (pvecintN sp _1 _2) (n, v)
    let parse n s = do ((_, v), s') <- (pvecintN sp _1 _2).fromPrinted ((n, def), s)
                       pure (v, s')

    prop "fromPrinted is inverse of toPrinted" $
      \xs -> let v = V.fromList xs ; n = V.length v in
        (print n v >>= parse n) `shouldBe` Just (v, "")
    it "consumes whitespace correctly" do
      parse 2 " 1  \n  2  " `shouldBe` Just (V.fromList [1,2], "  ")


  describe "large parser" do
    let p = (pint _1 <> sp <> pint _2 <> endl)
            <> pvecvecint _1 _2 _3
            <> (pint _4 <> endl)
            <> pvecN endl _4 _5 (pint _1 <> sp <> pint _2)
    let print = toPrinted' p
    let parse s = p.fromPrinted ((0, 0, V.empty, 0, V.empty), s)

    it "simple" do
      parse "1 2\n0 0\n2\n1 1\n2 2" `shouldBe` Just ((1, 2, V.singleton $ V.fromList [0, 0], 2, V.fromList [(1,1), (2,2)]), "")

    let input = do
          m <- sized $ \s -> chooseInt (1, 1 `max` s)
          mat <- listOf1 (vectorOf m arbitrary)
          qs <- listOf arbitrary
          pure (length mat, m, V.fromList $ map V.fromList mat, length qs, V.fromList qs)

    prop "fromPrinted is inverse of toPrinted" $ forAll input
      \x -> (print x >>= parse) == Just (x, "")

    it "consumes whitespace correctly" do
      parse " 1   2 \n 0   0 2  1 1  \n2   2  \n" `shouldBe` Just ((1, 2, V.singleton $ V.fromList [0, 0], 2, V.fromList [(1,1), (2,2)]), "  \n")
