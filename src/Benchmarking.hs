module Benchmarking 
	(
		benchFactorization,
		benchMain
	) where

import Criterion.Main
import Math.NumberTheory.Factor ( pfactors )
import Math.NumberTheory.Primes
import Lib

benchFactorization :: Int -> [Benchmark]
benchFactorization n = [
    -- bench "fib(n)" $ (nf fib) n,
    bench "pfactors(fibn)" $ (nf pfactors) fibn, 
    bench "factorise(fibn)" $ (nf factorise) fibn ]
    -- bench "fibPsp(n, fibn, fibn_factors)" $ (nf fibPspNoFactors) factors ]
    where 
        fibn = fib n
        -- factors = (n, fibn, pfactors(fibn))

benchMain :: IO ()
benchMain = defaultMain $ map (\n -> bgroup ("SchaeferFibgen " ++ show n) $ benchFactorization n) [1, 2 .. 100]
-- bgroup "SchaeferFibgen [1, 100)" $ benchSuite [1, 2 .. 99],
-- bgroup "SchaeferFibgen [100, 200)" $ benchSuite [100, 101 .. 199]