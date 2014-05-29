{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TupleSections         #-}
module FineHeap (TheHeap, FineHeap, testIt) where

import Control.Applicative
import Control.Concurrent.STM
import Control.Monad
import Data.List hiding (elem)
import Data.Ord
import Prelude hiding (elem)
import Test.QuickCheck (Args (..), Property, quickCheckWith, stdArgs)
import Test.QuickCheck.Monadic (monadicIO, run)
import qualified Test.QuickCheck.Monadic (assert)

import ConcurrentHeap
import FineHeapUnsafe (FineHeapUnsafe)

type TheHeap = FineHeap

data FineHeap e = FineHeap {
  heapHeap  :: FineHeapUnsafe e
, heapPutIn :: TVar Int
, heapPopIn :: TVar Int
}

incTVar, decTVar :: TVar Int -> STM ()
incTVar = flip modifyTVar' (+ 1)
decTVar = flip modifyTVar' (subtract 1)

-- This heap has a full mutual exclusion but no fairness guarantees.
instance (Ord e) => ConcurrentHeap (FineHeap e) e where
  heapNew n = liftM3 FineHeap (heapNew n) (newTVarIO 0) (newTVarIO 0)

  heapPut (FineHeap root putin popin) new = do
    atomically $ do
      popin' <- readTVar popin
      check $ popin' == 0
      incTVar putin
    heapPut root new
    atomically $ decTVar putin

  heapPop (FineHeap root putin popin) = do
    atomically $ do
      putin' <- readTVar putin
      check $ putin' == 0
      incTVar popin
    e <- heapPop root
    atomically $ decTVar popin
    return e

testSorting :: [Int] -> Property
testSorting xs = monadicIO $ do
  sxs <- run $ do
    let l = length xs
    h <- heapNew l :: IO (FineHeap Int)
    mapM_ (heapPut h) xs
    replicateM l (heapPop h)
  Test.QuickCheck.Monadic.assert $ sxs == sort xs

testIt :: IO ()
testIt = quickCheckWith (stdArgs { maxSuccess = 10000 }) testSorting

