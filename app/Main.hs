module Main where

import Lib
import System.TimeIt

main :: IO ()
main = print $ factory [1, 2 .. 100]

factory :: [Int] -> [(Integer, [Integer], [Integer])]
factory targets = map (timeIt . carlTest) targets 