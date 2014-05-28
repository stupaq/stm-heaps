{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE UndecidableInstances   #-}
module ConcurrentHeap where

import Control.Concurrent.STM
import Control.Exception (assert)

class (Ord e) => ConcurrentHeap h e | h -> e where
  heapNew :: Int -> IO h
  heapPut :: h -> e -> IO ()
  heapPop :: h -> IO e
  heapBuild :: Int -> [e] -> IO h
  heapBuild cap xs = do
    h <- heapNew cap
    mapM_ (heapPut h) $ assert (length xs <= cap) xs
    return h

class (Ord e) => BoundedHeap h e | h -> e where
  blockingPut :: h -> e -> IO ()
  blockingPop :: h -> IO e

data BoundedDecorator h = BoundedDecorator {
  blockingHeap     :: h
, blockingCapacity :: Int
, blockingSize     :: TVar Int
}

instance (Ord e, ConcurrentHeap h e) => BoundedHeap (BoundedDecorator h) e where
  --TODO
  blockingPut = undefined
  blockingPop = undefined

