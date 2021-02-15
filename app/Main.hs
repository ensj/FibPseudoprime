module Main where

import Lib
import Math.NumberTheory.Factor ( pfactors )
import Math.NumberTheory.Primes
import Data.Text as T
import Data.Text.IO as T.IO
import Data.List as L

printFibFactors :: (Int, Integer, [Integer]) -> IO ()
printFibFactors (n, fibn, factors) = do 
    print(show n ++ "th Fib: " ++ show fibn)
    print("Factors:  " ++ show factors ++ "  --  length: " ++ show (L.length factors))

main :: IO ()
main = T.IO.writeFile "output/output.txt" 
        (T.map (\c -> if c == '(' then '[' else if c == ')' then ']' else c) $ 
            T.pack $
            show $ L.map carlTest [1, 2 .. 300])
