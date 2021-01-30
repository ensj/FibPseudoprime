module Main where

import Lib
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
    print(pfactors(fibn))
