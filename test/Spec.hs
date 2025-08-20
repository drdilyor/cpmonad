module Main where

import Test.Hspec

import Cpmonad.PrinterSpec qualified

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "Cpmonad.PrinterSpec" Cpmonad.PrinterSpec.spec
