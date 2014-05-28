{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module FineHeap where

import Control.Applicative
import Control.Concurrent.STM
import Control.Monad
import Data.List
import Data.Ord
import Data.Tuple
import Test.QuickCheck (Property, quickCheck)
import Test.QuickCheck.Monadic (monadicIO, run)
import qualified Test.QuickCheck.Monadic (assert)

import ConcurrentHeap

type TheHeap = FineHeap

data Tree e = Nil | Root {
  treeCount :: TVar Int
, treeElem  :: TVar (Maybe e)
, treeLeft  :: Tree e
, treeRight :: Tree e
}

treeNew :: Int -> IO (Tree e, Int)
treeNew n
  | n > 0 = do
    s <- newTVarIO 0
    e <- newTVarIO Nothing
    let m = n `quot` 2
    [(tl, nl), (tr, nr)] <- replicateM 2 $ treeNew m
    return (Root s e tl tr, nl + nr + 1)
  | otherwise = return (Nil, 0)

treePut :: (Ord e) => Tree e -> e -> IO ()
treePut (Root size elem left right) new = do
  next <- atomically $ do
    sz <- readTVar size
    el <- readTVar elem
    writeTVar size (sz + 1)
    case (sz, el) of
      (0, _) -> do
        writeTVar elem $ Just new
        return Nothing
      (_, Nothing) -> retry
      (_, Just el) -> do
        writeTVar elem $ Just $ minimum [el, new]
        lsz <- treeSize left
        rsz <- treeSize right
        return $ Just (if lsz < rsz then left else right, maximum [el, new])
  case next of
    Nothing -> return ()
    Just (tree, ins) -> treePut tree ins
    where
      putElem (Root size elem left right) new = do
        undefined
-- FIXME reserve place in the subtree when choosing the smaller one

treeSize :: Tree e -> STM Int
treeSize Nil = return 0
treeSize (Root size _ _ _) = readTVar size

treePop :: (Ord e) => Tree e -> IO e
treePop root@(Root size elem left right) = do
  el <- atomically $ do
    sz <- readTVar size
    check $ sz > 0
    writeTVar size (sz - 1)
    el <- readTVar elem
    case el of
      Nothing -> retry
      Just el -> do
        writeTVar elem Nothing
        return el
  pushGap root
  return el

pushGap :: (Ord e) => Tree e -> IO ()
pushGap (Root size elem left right) = do
  next <- atomically $ do
    -- size has been decremented by the caller
    sz <- readTVar size
    el <- readTVar elem
    case (sz, el) of
      (0, _) -> return Nothing
      (_, Nothing) -> do
        [lel, rel] <- mapM (readTVar . treeElem) [left, right]
        case (lel, rel) of
          (Nothing, _) -> retry
          (_, Nothing) -> retry
          (Just lel, Just rel) ->
            let (tree, el) = minimumBy (comparing snd) [(left, lel), (right, rel)] in do
              writeTVar elem $ Just el
              writeTVar (treeElem tree) Nothing
              return $ Just tree
  case next of
    Nothing -> return ()
    Just tree -> pushGap tree

data FineHeap e = FineHeap {
  heapCapacity :: Int
, heapRoot     :: Tree e
}

instance (Ord e) => ConcurrentHeap (FineHeap e) e where
  heapNew size = uncurry FineHeap <$> swap <$> treeNew size

  heapPut = undefined

  heapPop = undefined

testSorting :: [Int] -> Property
testSorting xs = monadicIO $ do
  sxs <- run $ do
    let l = length xs
    h <- heapNew l :: IO (FineHeap Int)
    mapM_ (heapPut h) xs
    replicateM l (heapPop h)
  Test.QuickCheck.Monadic.assert $ sxs == sort xs

testIt = quickCheck testSorting

