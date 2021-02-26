module TimeLimits
  ( 
    timeLimited
  ) where

import Control.Concurrent.STM
import Control.DeepSeq
import System.Timeout

type Time = Int

-- | Compute as many items of a list in given timeframe (microseconds)
--   This is done by running a function that computes (with `force`)
--   list items and pushed them onto a `TVar [a]`.  When the requested time
--   expires, ghc will terminate the execution of `forceIntoTVar`, and we'll
--   return what has been pushed onto the tvar.
timeLimited :: (NFData a) => Time -> Int -> [a] -> IO [a]
timeLimited t x xs = do
    v <- newTVarIO []
    results <- timeout t (forceIntoTVar xs v)
    case results of
        Just a -> return ()
        Nothing -> print("Complete factorization failed at F_{" ++ show x ++ "}.")
    readTVarIO v 

-- | Force computed values into given tvar
forceIntoTVar :: (NFData a) => [a] -> TVar [a] -> IO [()]
forceIntoTVar xs v = mapM (atomically . modifyTVar v . forceCons) xs

-- | Returns function that does actual computation and cons' to tvar value
forceCons :: (NFData a) => a -> [a] -> [a]
forceCons x = (force x:)