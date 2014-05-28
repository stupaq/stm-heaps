import Control.Applicative
import Control.Concurrent
import Control.Concurrent.Async
import Control.Monad
import System.Random
import System.TimeIt
import Text.Printf (printf)

import ConcurrentHeap
--import CoarseHeap as Heap
import FineHeap as Heap

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

main :: IO ()
main = do
  setStdGen $ mkStdGen seed
  Heap.testIt
  numCores <- getNumCapabilities
  let numReaders = 1
  let numWriters = maximum [1, numCores - numReaders]
  h <- (heapBuild capacity <$> take initSize <$> randoms =<< newStdGen) :: IO (Heap.TheHeap Int)
  (time, _) <- timeItT $ do
    writers <- spawnThreads (writerThread h) initSize numWriters
    readers <- spawnThreads (readerThread h) initSize numReaders
    mapM_ wait $ writers ++ readers
  printf "+++ OK, time %.2f s, %d cores %d writers %d readers\n" time numCores numWriters numReaders

