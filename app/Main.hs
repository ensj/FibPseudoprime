module Main where

import Lib
import Sort
import Data.List
import Math.NumberTheory.Factor

main :: IO ()
main = do
    print("Input a fib factor to calculate.")
    input <- getLine
    let n = (read input :: Int)
    let fibn = fib n
    print(input ++ "th fib number: ")
    print(fibn)
    print("factors:")
    let pf = pfactors(fibn)
    let m = toInteger(n)
    print(filter (\factor -> (factor `mod` m == 1) || (factor `mod` m == m - 1)) pf)
    --let subs = subsets(pf)
    --print(subs)
    --let spbl = splitByLength(subs)
    --print(spbl)
