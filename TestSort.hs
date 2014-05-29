{-# LANGUAGE CPP                   #-}
{-# LANGUAGE MultiParamTypeClasses #-}

import Control.Applicative
import Control.Concurrent
import Control.Concurrent.Async
import Control.Concurrent.STM
import Control.Concurrent.STM.TArray
import Control.Monad
import Data.Array.MArray
import Data.List (sort)
import Data.Maybe (isJust)
import System.Random
import System.TimeIt
import Text.Printf (printf)

import ConcurrentHeap
import FineHeapUnsafe

inserterThread :: FineHeapUnsafe Int -> [Int] -> IO ()
inserterThread heap = mapM_ (heapPut heap)

removerThread :: FineHeapUnsafe Int -> TArray Int (Maybe Int) -> Int -> Int -> Int -> IO ()
removerThread heap output totalSize numCores myNum
  | myNum > totalSize = return ()
  | otherwise = do
    atomically $ do
      prev <- readArray output (myNum - 1)
      check $ isJust prev
      el <- heapPopWithGap heap
      writeArray output myNum $ Just el
    heapUngap heap
    removerThread heap output totalSize numCores (myNum + numCores)

main :: IO ()
main = do
  setStdGen $ mkStdGen 123
  numCores <- getNumCapabilities
  let perCore = 100000 `quot` numCores
  let totalSize = perCore * numCores
  inputs <- replicateM numCores $ replicateM perCore (randomIO :: IO Int)
  heap <- heapNew (2 * totalSize) :: IO (TheHeap Int)
  output <- atomically $ newArray (0, totalSize) Nothing :: IO (TArray Int (Maybe Int))
  atomically $ writeArray output 0 (Just 0)
  (time, _) <- timeItT $ do
    mapM_ wait =<< forM inputs (async . inserterThread heap)
    mapM_ wait =<< forM [1..numCores] (async . removerThread heap output totalSize numCores)
  result <- tail <$> atomically (getElems output)
  let reference = sort $ map Just $ concat inputs
  printf "+++ %s, time %.2f s, %d processes\n" (if result == reference then "OK" else "BAD") time numCores

