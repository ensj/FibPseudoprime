module Main where

import Lib
import Sort
import Data.List (filter, product)
import Math.NumberTheory.Factor
import Math.NumberTheory.Primes.Testing

main :: IO Integer
main = do
    print("Input starting fib index (n)")
    input <- getLine
    let n = (read input :: Int)

    let fibFunc = fibFactorsCleaned n

    print("Input ending fib index (n + m)")
    input <- getLine
    let m = (read input :: Int)

    fibFunc(n + m)

fibFactorsCleaned :: Int -> Int -> IO Integer
fibFactorsCleaned n limit = do
    if n /= limit 
    then do
        let fibn = fib n
        ---ADD A removeDuplicate FUNCTION AND CALL IT HERE. WE NEED TO GET RID OF DUPLICATE FACTORS
        let primeFactor = pfactors(fibn)
        --DUPLICATES SHOULD IDEALLY BE DISCARDED HERE
        let ntoi = toInteger(n) -- n to Integer
        let pfFiltered = filter (\factor -> (factor `mod` ntoi == 1) || (factor `mod` ntoi == ntoi - 1)) primeFactors
        --DISCARDING THEM HERE WORKS TOO
        -- print("Filtered factors: " ++ show pfFiltered)

        -- clean to ([+/- 1 mod 5], [+/- 2 mod 5])
        let cl = cleanList pfFiltered
        -- print("cleanList: " ++ show cl)

        -- (map product) +/- 1 mod 5 subsets
        let oneModFive = map product $ filter (\set -> set /= []) $ subsets $ snd cl
        -- print("One mod five: " ++ show oneModFive)

        let singletonOddProd = splitByLength $ filter (\set -> set /= []) $ subsets $ fst cl
        -- print("Single moms in your area: " ++ show singletonOddProd)


        let fibpsp = concat [cartesianProduct(oneModFive, fst singletonOddProd), 
                             snd singletonOddProd, 
                             cartesianProduct(fst singletonOddProd, snd singletonOddProd)
                             ]

        if length fibpsp == 0 
        then do
            print("No candidates from F_{" ++ show n ++ "}.")
            fibFactorsCleaned (n+1) limit
        else do
            print("Viable candidates generated from F_{" ++ show n ++ "} (" ++ show fibn ++ "): ")
            print fibpsp

            let baseTwoFibpsp = filter (\elem -> isFermatPP elem 2) fibpsp

            if length baseTwoFibpsp /= 0
            then do
                print "Base-2 Pseudoprime found!"
                print(baseTwoFibpsp)
                return (-1)
            else fibFactorsCleaned (n+1) limit
    else return (-1)
