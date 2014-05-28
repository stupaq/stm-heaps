{-# LANGUAGE MultiParamTypeClasses #-}

import Control.Applicative
import Control.Concurrent
import Control.Concurrent.Async
import Control.Monad
import System.Random
import System.TimeIt
import Text.Printf (printf)

import CoarseHeap as Heap
import ConcurrentHeap
--import FineHeap as Heap

seed, capacity, initSize :: Int
seed = 123
capacity = 200000
initSize = capacity `quot` 2

readerThread, writerThread :: (ConcurrentHeap h a, Random a) => h -> Int -> IO ()
readerThread heap num =
  replicateM_ num $ heapPop heap
writerThread heap num = do
  elems <- take num <$> randoms <$> newStdGen
  forM_ elems $ heapPut heap

spawnThreads :: (Int -> IO a) -> Int -> Int -> IO [Async a]
spawnThreads thread work num = replicateM num (async $ thread $ work `quot` num)

main :: IO ()
main = do
  Heap.testIt
  scenario1
  scenario2
  scenario3
  scenario4

scenario1, scenario2, scenario3, scenario4 :: IO ()
scenario1 = do
  setStdGen $ mkStdGen seed
  numCores <- getNumCapabilities
  let numReaders = 1
  let numWriters = maximum [1, numCores - numReaders]
  h <- (heapBuild capacity <$> take initSize <$> randoms =<< newStdGen) :: IO (Heap.TheHeap Int)
  (time, _) <- timeItT $ do
    writers <- spawnThreads (writerThread h) initSize numWriters
    readers <- spawnThreads (readerThread h) initSize numReaders
    mapM_ wait $ writers ++ readers
  printf "+++ OK, time %.2f s, %d cores %d writers %d readers\n" time numCores numWriters numReaders

scenario2 = do
  setStdGen $ mkStdGen seed
  numWriters <- getNumCapabilities
  h <- (heapBuild capacity <$> take initSize <$> randoms =<< newStdGen) :: IO (Heap.TheHeap Int)
  (time, _) <- timeItT $ do
    writers <- spawnThreads (writerThread h) initSize numWriters
    mapM_ wait writers
  printf "+++ OK, time %.2f s, %d writers\n" time numWriters

scenario3 = do
  setStdGen $ mkStdGen seed
  numReaders <- getNumCapabilities
  h <- (heapBuild capacity <$> take initSize <$> randoms =<< newStdGen) :: IO (Heap.TheHeap Int)
  (time, _) <- timeItT $ do
    readers <- spawnThreads (readerThread h) initSize numReaders
    mapM_ wait readers
  printf "+++ OK, time %.2f s, %d readers\n" time numReaders

scenario4 = do
  setStdGen $ mkStdGen seed
  h <- (heapBuild capacity <$> take initSize <$> randoms =<< newStdGen) :: IO (Heap.TheHeap Int)
  (time, _) <- timeItT $ do
    writers <- spawnThreads (writerThread h) initSize 1
    readers <- spawnThreads (readerThread h) initSize 1
    mapM_ wait $ writers ++ readers
  printf "+++ OK, time %.2f s, sequentially\n" time

