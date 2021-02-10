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

prop_powersetLength :: [Integer] -> Bool
prop_powersetLength l = 
    (length $ subsets l) == 2^(length l)

main :: IO ()
main = hspec $ do
    describe "Top" $ do 
        describe "Research Functions Test Suite" $ do
            prop "Check that fib n correctly returns the nth fib number" $
                prop_fibEqualsfib
        describe "Bottom" $ do
            prop "Subsets returns the proper number of powersets" $ 
                prop_powersetLength
        
