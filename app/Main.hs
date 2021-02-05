module Main where

import Lib
import Sort
import Data.List (filter, product)
import Math.NumberTheory.Factor
import Math.NumberTheory.Primes.Testing

main :: IO Integer
main = do
    print("Input a fib factor to calculate From. (Calculates n to n + 500)")
    input <- getLine
    let n = (read input :: Int)

    let fibFunc = fibFactorsCleaned n
    fibFunc(n + 500)

fibFactorsCleaned :: Int -> Int -> IO Integer
fibFactorsCleaned n limit = do
    if n /= limit 
    then do
        let fibn = fib n

        let primeFactors = pfactors(fibn)
        let ntoi = toInteger(n) -- n to Integer
        let pfFiltered = filter (\factor -> (factor `mod` ntoi == 1) || (factor `mod` ntoi == ntoi - 1)) primeFactors

        -- clean to ([+/- 1 mod 5], [+/- 2 mod 5])
        let cl = cleanList(pfFiltered)

        -- (map product) +/- 1 mod 5 subsets, (singleton factors, and odd products)
        let oneModFive = filter (\prod -> prod /= 1) (map product (subsets(fst cl)))
        let singletonOddProd = splitByLength(subsets(snd cl))

        let fibpsp = concat [cartesianProduct(oneModFive, fst singletonOddProd), 
                             snd singletonOddProd, 
                             cartesianProduct(fst singletonOddProd, snd singletonOddProd)
                             ]

        if length fibpsp == 0 
        then do
            print("No fibpseudoprimes! " ++ show n)
            fibFactorsCleaned (n+1) limit
        else do
            print(show n ++ "th fib number: " ++ show fibn)
            print "FibPsp:"
            print fibpsp

            let baseTwoFibpsp = filter (\elem -> isFermatPP elem 2) fibpsp

            if length baseTwoFibpsp /= 0
            then do
                print "Base two pseudoprime psp:"
                print(baseTwoFibpsp)
                return (-1)
            else fibFactorsCleaned (n+1) limit
    else return (-1)
