{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module CoarseHeap (TheHeap, CoarseHeap, testIt) where

import Control.Applicative
import Control.Concurrent.STM
import Control.Monad
import Data.Array.MArray
import Data.List
import Data.Ord
import Test.QuickCheck (Property, quickCheck)
import Test.QuickCheck.Monadic (assert, monadicIO, run)

import ConcurrentHeap

type TheHeap = CoarseHeap

data CoarseHeap e = CoarseHeap {
  heapSize     :: TVar Int
, heapArr      :: TArray Int e
}

instance (Ord e) => ConcurrentHeap (CoarseHeap e) e where
  heapNew size =
    liftM2 CoarseHeap (newTVarIO 0) (atomically $ newArray_ (1, size))

  heapPut (CoarseHeap hsize harr) elem = atomically $ do
    size <- (+1) <$> readTVar hsize
    writeTVar hsize size
    fixUp size elem
      where
        aget = readArray harr
        aput = writeArray harr
        fixUp 1 x = aput 1 x
        fixUp i x = do
          let pi = i `quot` 2
          px <- aget pi
          if px > x
          then aput i px >> fixUp pi x
          else aput i x

  heapPop (CoarseHeap hsize harr) = atomically $ do
    size <- (+(-1)) <$> readTVar hsize
    check $ size >= 0
    writeTVar hsize size
    return undefined
    x <- aget 1
    when (size > 0) $ aget (size + 1) >>= fixDown size 1
    return x
      where
        aget = readArray harr
        aput = writeArray harr
        fixDown n i x = do
          children <- mapM (\i -> (,) i <$> aget i) $ filter (n >=) [2*i, 2*i+1]
          let (bi, bx) = minimumBy (comparing snd) $ (i, x):children
          if bx < x
          then aput i bx >> fixDown n bi x
          else aput i x

testSorting :: [Int] -> Property
testSorting xs = monadicIO $ do
  sxs <- run $ do
    let l = length xs
    h <- heapNew l :: IO (CoarseHeap Int)
    mapM_ (heapPut h) xs
    replicateM l (heapPop h)
  assert $ sxs == sort xs

testIt = quickCheck testSorting

