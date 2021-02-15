module Main where

import Lib
import Math.NumberTheory.Factor ( pfactors )
import Math.NumberTheory.Primes
import Data.Text (pack)
import Data.Text.IO ( writeFile )
import Data.List

printFibFactors :: (Int, Integer, [Integer]) -> IO ()
printFibFactors (n, fibn, factors) = do 
    print(show n ++ "th Fib: " ++ show fibn)
    print("Factors:  " ++ show factors ++ "  --  length: " ++ show (length factors))

main :: IO ()
main = Data.Text.IO.writeFile "output/output.txt" (pack $ show $ map carlTest [1, 2 .. 300])
