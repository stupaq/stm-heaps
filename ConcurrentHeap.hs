{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
module ConcurrentHeap where

import qualified Control.Exception (assert)

class (Ord e) => ConcurrentHeap h e | h -> e where
  heapNew :: Int -> IO h
  heapPut :: h -> e -> IO ()
  heapPop :: h -> IO e
  heapBuild :: Int -> [e] -> IO h
  heapBuild cap xs = do
    h <- heapNew cap
    mapM_ (heapPut h) $ Control.Exception.assert (length xs <= cap) xs
    return h

