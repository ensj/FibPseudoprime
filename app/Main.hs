module Main where

import Lib
import System.TimeIt

main :: IO ()
main = do 
    print "start"
    timeIt $ print $ factory [1, 2 .. 100]
    print "end"

factory :: [Int] -> [(Integer, [Integer], [Integer])]
factory targets = map (carlTest) targets 