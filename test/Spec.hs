module Main where

import Lib
import Sort
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck
import Control.Exception (evaluate)
import Debug.Trace

numbers :: [Int]
numbers = [1..1000]
 
genNumbers :: Gen Int
genNumbers = elements numbers

prop_fibEqualsfib :: Property
prop_fibEqualsfib =
  forAll genNumbers
    (\x ->
      fib x == classicFib x)

main :: IO ()
main = hspec $ do
    describe "Fibonacci" $ do
        prop "Check that fib n correctly returns the nth fib number" $
            prop_fibEqualsfib

    describe "Sort" $ do
        prop "Blach"