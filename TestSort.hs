{-# LANGUAGE CPP                   #-}
{-# LANGUAGE MultiParamTypeClasses #-}

import Control.Applicative
import Control.Concurrent
import Control.Concurrent.STM
import Control.Concurrent.Async
import Data.Array.MArray
import Control.Monad
import System.Random
import System.TimeIt
import Text.Printf (printf)

import ConcurrentHeap

import FineHeapUnsafe

main :: IO ()
main = do
  setStdGen $ mkStdGen 123
  numCores <- getNumCapabilities
  let perCore = 100000 `quot` numCores
  let totalSize = perCore * numCores
  inputs <- replicateM numCores $ replicateM perCore (randomIO :: IO Int)
  heap <- heapNew (2 * totalSize) :: IO (TheHeap Int)
  (time, _) <- timeItT $ do
    undefined
  printf "+++ OK, time %.2f s, %d processes\n" time numCores



