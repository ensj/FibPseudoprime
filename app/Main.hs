module Main where

import Lib
import Math.NumberTheory.Primes
import Data.Text as T
import Data.Text.IO as T.IO
import Data.List as L
import TimeLimits

printFibFactors :: (Int, Integer, [Integer]) -> IO ()
printFibFactors (n, fibn, factors) = do 
    print(show n ++ "th Fib: " ++ show fibn)
    print("Factors:  " ++ show factors ++ "  --  length: " ++ show (L.length factors))

hundred :: Int -> [Int]
hundred n = [n * 100 + 1, (n * 100) + 2 .. (n * 100) + 100]

createOutputOriginal :: Int -> IO()
createOutputOriginal n = do
    T.IO.writeFile ("output/output_" ++ show (n * 100 + 1) ++ "-" ++ show (n * 100 + 100) ++ ".txt") 
        --  $ T.map (\c -> if c == '(' then '[' else if c == ')' then ']' else c)
            $ T.pack
            $ show $ L.map carlTest $ hundred n
    print ("Finished [" ++ show (n * 100 + 1) ++ " .. " ++ show (n * 100 + 100) ++ "].")

createOutputTermLimits :: Int -> IO()
createOutputTermLimits n = do
    factorList <- mapM carlTestTermLimits $ hundred n
    T.IO.writeFile ("output/output_" ++ show (n * 100 + 1) ++ "-" ++ show (n * 100 + 100) ++ ".txt") 
        --  $ T.map (\c -> if c == '(' then '[' else if c == ')' then ']' else c)
            $ T.pack
            $ show $ factorList
    print ("Finished [" ++ show (n * 100 + 1) ++ " .. " ++ show (n * 100 + 100) ++ "].")


main :: IO ()
main = mapM_ (createOutputTermLimits) [5, 6 .. ]
