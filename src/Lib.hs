module Lib
    ( 
    classicFib, 
    fib,
    factory
    ) where

import Sort ( cleanList, subsets, splitByLength, cartesianProduct, removeDuplicates )
import Data.List ( foldl' )
import Math.NumberTheory.Factor ( pfactors )
import Data.Bits ( Bits(testBit) )
import Math.NumberTheory.Primes.Testing ( isFermatPP )

fibs = 0 : 1 : zipWith (+) fibs (tail fibs)

classicFib :: Int -> Integer 
classicFib n = fibs!!n

fib :: Int -> Integer
fib n = snd . foldl_ fib_ (1, 0) . dropWhile not $
            [testBit n k | k <- let s = n in [s-1,s-2..0]]    
    where
        fib_ (f, g) p
            | p         = (f*(f+2*g), ss)
            | otherwise = (ss, g*(2*f-g))
            where ss = f*f+g*g
        foldl_ = foldl' -- '

-- generate fibpsp for the nth fib number
fibPsp :: Int -> (Integer, [Integer])
fibPsp n = 
    (fibn, psp) where
        fibn = fib n
        -- get factors for nth fib number
        primeFactors = pfactors(fibn)
        ntoi = toInteger(n)
        -- calculates the prime factors of fib(n) that are +-1 mod n
        pfFiltered = filter (\factor -> (factor `mod` ntoi == 1) || (factor `mod` ntoi == ntoi - 1)) primeFactors
        -- clean factors to ([+- 1 mod 5], [+- 2 mod 5])
        cl = cleanList pfFiltered
        -- get all multiples of [+- 1 mod 5] factors
        oneModFiveSets = map product $ filter (\set -> set /= []) $ subsets $ fst cl
        -- get all odd multiples of [+- 2 mod 5] factors
        (singleton, oddMultiple) = splitByLength $ filter (\set -> set /= []) $ subsets $ snd cl
        -- the concatenation of the cartesian product of oneModFive and singleton, 
        -- the odd products by themselves, and the cartesian product of singleton and
        -- the odd products form the list of fibonacci pseudoprimes.
        psp = removeDuplicates $ 
                concat [cartesianProduct(singleton, oneModFiveSets), 
                        cartesianProduct(oddMultiple, oneModFiveSets), 
                        oddMultiple]

factory :: Int -> Int -> IO Integer
factory n limit = do
    if n /= limit 
    then do
        let (fibn, fibpsp) = fibPsp n
        -- If a pseudoprime doesn't exist
        if length fibpsp /= 0 
        then do
            -- then list the viable candidates.
            print("Viable candidates generated from F_{" ++ show n ++ "} (" ++ show fibn ++ "): ")
            print fibpsp
            print("Length: " ++ show (length fibpsp))

            -- perform the base-2 pseudoprime test.
            let baseTwoFibpsp = filter (\elem -> isFermatPP elem 2) fibpsp

            -- if a base-2 pseudoprime exists
            if length baseTwoFibpsp /= 0
            then do
                -- then state one (or more) has been found.
                print "Base-2 Pseudoprime found!"
                print(baseTwoFibpsp)
                return (-1)
            else factory (n + 1) limit
        else do
            -- else state there are no candidates for the base-2 pseudoprime test.
            print("No candidates from F_{" ++ show n ++ "}.")
            factory (n + 1) limit
    else return (-1)