{-# LANGUAGE CPP                   #-}
{-# LANGUAGE MultiParamTypeClasses #-}

import Control.Applicative
import Control.Concurrent
import Control.Concurrent.Async
import Control.Monad
import System.Random
import System.TimeIt
import Text.Printf (printf)

import ConcurrentHeap

#ifdef HEAP_VERSION
import HEAP_VERSION as Heap
#else
--import CoarseHeap as Heap
import FineHeap as Heap
#endif

seed, capacity, initSize :: Int
seed = 123
capacity = 100000
initSize = capacity `quot` 2

readerThread, writerThread :: (ConcurrentHeap h a, Random a) => h -> Int -> IO ()
readerThread heap num =
  replicateM_ num $ heapPop heap
writerThread heap num = do
  elems <- take num <$> randoms <$> newStdGen
  forM_ elems $ heapPut heap

spawnThreads :: (Int -> IO a) -> Int -> Int -> IO [Async a]
spawnThreads thread work num = replicateM num (async $ thread $ work `quot` num)

type Scenario = Int -> Heap.TheHeap Int -> IO ()
runScenario :: Scenario -> IO ()
runScenario sc = do
  setStdGen $ mkStdGen seed
  numCores <- getNumCapabilities
  heap <- (heapBuild capacity <$> take initSize <$> randoms =<< newStdGen) :: IO (Heap.TheHeap Int)
  sc numCores heap

main :: IO ()
main = do
  Heap.testIt
  runScenario scenario1
  runScenario scenario2
  runScenario scenario3
  runScenario scenario4

scenario1, scenario2, scenario3, scenario4 :: Scenario
scenario1 numCores heap = do
  let numReaders = 1
  let numWriters = maximum [1, numCores - numReaders]
  (time, _) <- timeItT $ do
    writers <- spawnThreads (writerThread heap) initSize numWriters
    readers <- spawnThreads (readerThread heap) initSize numReaders
    mapM_ wait $ writers ++ readers
  printf "+++ OK, time %.2f s, %d cores %d writers %d readers\n" time numCores numWriters numReaders

scenario2 numWriters heap = do
  (time, _) <- timeItT $ do
    writers <- spawnThreads (writerThread heap) initSize numWriters
    mapM_ wait writers
  printf "+++ OK, time %.2f s, %d writers\n" time numWriters

scenario3 numReaders heap = do
  (time, _) <- timeItT $ do
    readers <- spawnThreads (readerThread heap) initSize numReaders
    mapM_ wait readers
  printf "+++ OK, time %.2f s, %d readers\n" time numReaders

scenario4 _ heap = do
  (time, _) <- timeItT $ do
    writers <- spawnThreads (writerThread heap) initSize 1
    mapM_ wait writers
    readers <- spawnThreads (readerThread heap) initSize 1
    mapM_ wait readers
  printf "+++ OK, time %.2f s, sequentially\n" time

