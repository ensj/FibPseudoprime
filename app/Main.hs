module Main where

import Lib

main :: IO Integer
main = do
    print("Input starting fib index (n)")
    input <- getLine
    let n = (read input :: Int)

    let fibFunc = factory n

    print("Input ending fib index (n + m)")
    input <- getLine
    let m = (read input :: Int)

    fibFunc(n + m)
