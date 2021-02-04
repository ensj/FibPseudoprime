module Sort
    ( cleanList,
      splitByLength,
      subsets
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

mapProductDebug :: [[Integer]] -> [Integer]
mapProductDebug n = m where
    m = map product n
