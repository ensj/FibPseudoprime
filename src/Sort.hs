module Sort
    ( cleanList,
      listProducts,
      subsets
    ) where

import Data.List ( filter, product )
import Prelude

cleanList :: [Integer] -> ([Integer], [Integer])
cleanList n = (a, b) where
    a = filter (\n -> n `mod` 5 == 2) n
    b = filter (\n -> n `mod` 5 == 3) n

listProducts :: ([Integer], [Integer]) -> [Integer]
listProducts (n, b) = x where
    x = filter (\n -> n `mod` 5 == 1) n

subsets :: [Int] -> [[Int]]
subsets []  = [[]]
subsets (x:xs) = subsets xs ++ map (x:) (subsets xs)