module Main where

import Test.Hspec

import Cpmonad.GenSpec qualified
import Cpmonad.PrinterSpec qualified

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "Cpmonad.GenSpec" Cpmonad.GenSpec.spec
  describe "Cpmonad.PrinterSpec" Cpmonad.PrinterSpec.spec
