module Main where

import Lib
import Criterion.Main
import Math.NumberTheory.Factor ( pfactors )

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
    bench "pfactors(fibn)" $ (nf pfactors) fibn,
    bench "fibPsp(n, fibn, fibn_factors)" $ (nf fibPspNoFactors) factors ]
    where 
        fibn = fib n
        factors = (n, fibn, pfactors(fibn))

main :: IO ()
main = defaultMain $
    map (\n -> bgroup ("SchaeferFibgen " ++ show n) $ benchSuiteSingle n) [200, 201 .. 250]
    -- bgroup "SchaeferFibgen [1, 100)" $ benchSuite [1, 2 .. 99],
    -- bgroup "SchaeferFibgen [100, 200)" $ benchSuite [100, 101 .. 199]
    