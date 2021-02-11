module Main where

import Lib
import Criterion.Main
import Math.NumberTheory.Factor ( pfactors )
import Math.NumberTheory.Primes

factory :: [Int] -> [(Integer, [Integer], [Integer])]
factory targets = map carlTest targets 

benchSuite :: [Int] -> [Benchmark]
benchSuite list = [
    bench "fib(n)" $ (nf (map fib)) list
    , bench "pfactors(fibn)" $ (nf (map pfactors)) fibn
    , bench "fibPsp(n, fibn, fibn_factors)" $ (nf (map fibPspNoFactors)) factors ]
    where 
        fibn = map fib list
        factors = zip3 list fibn (map (\x -> pfactors(x)) fibn)

benchSuiteSingle :: Int -> [Benchmark]
benchSuiteSingle n = [
    -- bench "fib(n)" $ (nf fib) n,
    bench "pfactors(fibn)" $ (nf pfactors) fibn ]
    -- bench "fibPsp(n, fibn, fibn_factors)" $ (nf fibPspNoFactors) factors ]
    where 
        fibn = fib n
        -- factors = (n, fibn, pfactors(fibn))

factoriseTest :: Int -> (Int, Integer, [(Prime Integer, Word)])
factoriseTest n = (n, fibn, factors) 
    where fibn = fib n
          factors = factorise fibn

printFactors :: (Int, Integer, [(Prime Integer, Word)]) -> IO ()
printFactors (n, fibn, factors) = do 
    print("Fib " ++ show n ++ ": (" ++ show fibn ++ ")")
    print("--- Factors: " ++ show factors)

main :: IO ()
main = defaultMain $ 
    map (\n -> bgroup ("SchaeferFibgen " ++ show n) $ benchSuiteSingle n) [100, 101 .. 200]

-- map (printFactors . factoriseTest) [100, 101 .. 200]
-- defaultMain $ map (\n -> bgroup ("SchaeferFibgen " ++ show n) $ benchSuiteSingle n) [100, 101 .. 200]
-- bgroup "SchaeferFibgen [1, 100)" $ benchSuite [1, 2 .. 99],
-- bgroup "SchaeferFibgen [100, 200)" $ benchSuite [100, 101 .. 199]