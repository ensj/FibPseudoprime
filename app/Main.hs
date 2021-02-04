module Main where

import Lib
import Sort
import Data.List (filter, product)
import Math.NumberTheory.Factor
-- import Math.NumberTheory.Primes.Testing

main :: IO Integer
main = do
    print("Input a fib factor to calculate From. (Calculates n to n + 5)")
    input <- getLine
    let n = (read input :: Int)

    let fibFunc = fibFactorsCleaned n
    fibFunc(n + 10)

fibFactorsCleaned :: Int -> Int -> IO Integer
fibFactorsCleaned n limit = do
    if n /= limit 
    then do
        print(show n ++ "th fib number: ")
        let fibn = fib n
        print(fibn)

        let pf = pfactors(fibn)
        let m = toInteger(n)
        let pfFiltered = filter (\factor -> (factor `mod` m == 1) || (factor `mod` m == m - 1)) pf

        let cl = cleanList(pfFiltered)

        print("Subsets:")
        let subs = (map product (subsets(fst cl)), splitByLength(subsets(snd cl)))
        print(subs)

        print("Singleton Fibpsp")
        print(cartesianProduct(fst subs, fst (snd subs)))

        --print("SplitByLength:")
        --let spbl = splitByLength(subs)
        --print(spbl)

        --print("CartesianProduct")
        --print(cartesianProduct(spbl))
        fibFactorsCleaned (n+1) limit
    else return (-1)
