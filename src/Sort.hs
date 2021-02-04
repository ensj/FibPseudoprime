module Sort
    ( cleanList,
      splitByLength,
      subsets,
      cartesianProduct
    ) where

import Data.List ( filter, product )
import Prelude

--Cleans list--
cleanList :: [Integer] -> ([Integer], [Integer])
cleanList n = (a, b) where
    a = filter (\n -> (n `mod` 5 == 2) || (n `mod` 5 == 3)) n
    b = filter (\n -> (n `mod` 5 == 1) || (n `mod` 5 == 4)) n


--Calculates all possible subsets--
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
cartesianProduct :: ([Integer], [Integer]) -> [Integer]
cartesianProduct (a, b) = x where
    x = map product (sequence [a, b])

