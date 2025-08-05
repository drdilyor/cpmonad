module Main where

import Test.Hspec

main :: IO ()
main = hspec do
  describe "hello" do
    it "works" do
      True `shouldBe` True
