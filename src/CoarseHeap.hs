{-# LANGUAGE DoAndIfThenElse       #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module CoarseHeap (TheHeap, CoarseHeap, testIt) where

import Control.Applicative
import Control.Concurrent.STM
import Control.Monad
import Data.Array.MArray
import Data.List hiding (elem)
import Data.Ord
import Prelude hiding (elem)
import Prelude hiding (elem)
import Test.QuickCheck (Args (..), Property, quickCheckWith, stdArgs)
import Test.QuickCheck.Monadic (monadicIO, run)
import qualified Test.QuickCheck.Monadic (assert)

import ConcurrentHeap

type TheHeap = CoarseHeap

data CoarseHeap e = CoarseHeap {
  heapSize :: TVar Int
, heapArr  :: TArray Int e
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
          let ai = i `quot` 2
          ax <- aget ai
          if ax > x
          then aput i ax >> fixUp ai x
          else aput i x

  heapPop (CoarseHeap hsize harr) = atomically $ do
    size <- (+(-1)) <$> readTVar hsize
    check $ size >= 0
    writeTVar hsize size
    x <- aget 1
    when (size > 0) $ aget (size + 1) >>= fixDown size 1
    return x
      where
        aget = readArray harr
        aput = writeArray harr
        fixDown n i x = do
          children <- mapM (\j -> (,) j <$> aget j) $ filter (n >=) [2*i, 2*i+1]
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
  Test.QuickCheck.Monadic.assert $ sxs == sort xs

testIt :: IO ()
testIt = quickCheckWith (stdArgs { maxSuccess = 10000 }) testSorting

