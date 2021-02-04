module Main where

import Lib
import Sort
import Data.List
import Math.NumberTheory.Factor
-- import Math.NumberTheory.Primes.Testing

main :: IO ()
main = do
    print("Input a fib factor to calculate From. (Calculates n to n + 500)")
    input <- getLine
    let n = (read input :: Int)

    let fibFunc = fibFactorsCleaned n
    fibFunc(n + 500)
    print ""

fibFactorsCleaned :: Int -> Int -> IO Integer
fibFactorsCleaned n limit = do
    if n /= limit 
    then do
        print(show n ++ "th fib number: ")
        let fibn = fib n
        print(fibn)

        --print("factors (+-1 mod n):")
        let pf = pfactors(fibn)
        let m = toInteger(n)
        let pfFiltered = filter (\factor -> (factor `mod` m == 1) || (factor `mod` m == m - 1)) pf
        --print(pfFiltered)

        --print("Subsets:")
        let subs = subsets(pfFiltered)
        --print(subs)

        --print("SplitByLength:")
        let spbl = splitByLength(subs)
        --print(spbl)

        print("CartesianProduct")
        print(cartesianProduct(spbl))
        fibFactorsCleaned (n+1) limit
    else return (-1)
