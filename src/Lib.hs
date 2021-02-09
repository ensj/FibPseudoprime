module Lib
    ( fib,
    factory
    ) where


import Sort ( cleanList, subsets, splitByLength, cartesianProduct )
import Data.List ( foldl' )
import Math.NumberTheory.Factor ( pfactors )
import Data.Bits ( Bits(testBit) )
import Math.NumberTheory.Primes.Testing ( isFermatPP )

fib :: Int -> Integer
fib n = snd . foldl_ fib_ (1, 0) . dropWhile not $
            [testBit n k | k <- let s = n in [s-1,s-2..0]]          --deleted 'bitsize' before n, might have repercussions. 
    where
        fib_ (f, g) p
            | p         = (f*(f+2*g), ss)
            | otherwise = (ss, g*(2*f-g))
            where ss = f*f+g*g
        foldl_ = foldl' -- '

factory :: Int -> Int -> IO Integer
factory n limit = do
    if n /= limit 
    then do
        let fibn = fib n
        ---ADD A removeDuplicate FUNCTION AND CALL IT HERE. WE NEED TO GET RID OF DUPLICATE FACTORS
        let primeFactors = pfactors(fibn)
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
            factory (n+1) limit
        else do
            print("Viable candidates generated from F_{" ++ show n ++ "} (" ++ show fibn ++ "): ")
            print fibpsp

            let baseTwoFibpsp = filter (\elem -> isFermatPP elem 2) fibpsp

            if length baseTwoFibpsp /= 0
            then do
                print "Base-2 Pseudoprime found!"
                print(baseTwoFibpsp)
                return (-1)
            else factory (n+1) limit
    else return (-1)