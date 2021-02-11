module Sort
    ( cleanList,
      splitByLength,
      subsets,
      cartesianProduct, 
      removeDuplicates
    ) where

import Data.List ( filter, product )
import Prelude

--Cleans a set into a pair of ([+-1 mod 5], [+-2 mod 5]) sets--
cleanList :: [Integer] -> ([Integer], [Integer])
cleanList n = (a, b) where
    a = filter (\n -> (n `mod` 5 == 1) || (n `mod` 5 == 4)) n
    b = filter (\n -> (n `mod` 5 == 2) || (n `mod` 5 == 3)) n


--Takes a set and returns its powerset--
subsets :: [Integer] -> [[Integer]]
subsets []  = [[]]
subsets (x:xs) = subsets xs ++ map (x:) (subsets xs)

--Filters and splits list into singletons and odd products--
--Remove the 'map product' in each case to see the original lists (useful for debugging)--
splitByLength :: [[Integer]] -> ([Integer], [Integer])
splitByLength n = (a, b) where
    a = map product (filter (\n -> length n == 1) n)
    b = map product (filter (\n -> (length n > 1) && (length n `mod` 2 == 1)) n)

--Pass all the odd products to this function, along with (map product (even factor subsets))--
--I don't know how to explain this one via text--
cartesianProduct :: ([Integer], [Integer]) -> [Integer]
cartesianProduct ([], b) = []
cartesianProduct (a, []) = []
cartesianProduct (a, b) = x where
    x = map product (sequence [a, b])

--Removes duplicate elements from a list.--
removeDuplicates :: [Integer] -> [Integer]
removeDuplicates [] = []
removeDuplicates (x:xs)   | x `elem` xs   = removeDuplicates xs
                | otherwise     = x : removeDuplicates xs
