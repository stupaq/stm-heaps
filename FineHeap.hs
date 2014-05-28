{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE TupleSections     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module FineHeap where

import Control.Applicative
import Control.Concurrent.STM
import Control.Monad
import Data.List
import Data.Ord
import Test.QuickCheck (Property, quickCheck)
import Test.QuickCheck.Monadic (monadicIO, run)
import qualified Test.QuickCheck.Monadic (assert)

import ConcurrentHeap

type TheHeap = FineHeap

data FineHeap e = Nil | FineHeap {
  heapSize :: TVar Int
, heapElem  :: TVar (Maybe e)
, heapLeft  :: FineHeap e
, heapRight :: FineHeap e
}

readNode :: FineHeap e -> STM (Int, Maybe e)
readNode (FineHeap size elem _ _) = liftM2 (,) (readTVar size) (readTVar elem)
readNode Nil = return (0, Nothing)

putElem :: (Ord e) => FineHeap e -> e -> STM (Maybe e)
putElem root@(FineHeap size elem _ _) new = do
  (sz, el) <- readNode root
  case (sz, el) of
    (0, Nothing) -> do
      -- This is an empty node, we can put our stuff here and finish
      writeTVar elem $ Just new
      return Nothing
    (_, Nothing) ->
      -- This is a gap, we'll wait for someone to get rid of it
      retry
    (_, Just el) -> do
      -- We swap the element to be inserted if necessary
      if new < el
      then writeTVar elem (Just new) >> return (Just el)
      else return (Just new)

putElemRec :: (Ord e) => FineHeap e -> e -> IO ()
putElemRec (FineHeap _ _ left right) new = do
  next <- atomically $ do
    -- We choose the smaller subtree for insertion
    [lsz, rsz] <- mapM (readTVar . heapSize) [left, right]
    let root = if lsz < rsz then left else right
    -- Update it
    fmap (root, ) <$> putElem root new
  -- And proceed recursively if necessary
  case next of
    Nothing -> return ()
    Just (root', new') -> putElemRec root' new'

pushGap :: (Ord e) => FineHeap e -> IO ()
pushGap (FineHeap size elem left right) = do
  next <- atomically $ do
    -- size has been decremented by the caller
    sz <- readTVar size
    el <- readTVar elem
    case (sz, el) of
      (0, _) -> return Nothing
      (_, Nothing) -> do
        [lel, rel] <- mapM (readTVar . heapElem) [left, right]
        case (lel, rel) of
          -- FIXME
          (Nothing, _) -> retry
          (_, Nothing) -> retry
          (Just lel, Just rel) ->
            let (tree, el) = minimumBy (comparing snd) [(left, lel), (right, rel)] in do
              writeTVar elem $ Just el
              writeTVar (heapElem tree) Nothing
              return $ Just tree
  case next of
    Nothing -> return ()
    Just tree -> pushGap tree

instance (Ord e) => ConcurrentHeap (FineHeap e) e where
  heapNew n
    | n > 0 = do
      s <- newTVarIO 0
      e <- newTVarIO Nothing
      let m = n `quot` 2 -- ceil((n - 1) / 2)
      [tl, tr] <- replicateM 2 $ heapNew m
      return $ FineHeap s e tl tr
    | otherwise = return Nil

  heapPut root@(FineHeap {}) new = do
    next <- atomically $ putElem root new
    case next of
      Nothing -> return ()
      Just new' -> putElemRec root new'

  heapPop root@(FineHeap size elem left right) = do
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

testSorting :: [Int] -> Property
testSorting xs = monadicIO $ do
  sxs <- run $ do
    let l = length xs
    h <- heapNew l :: IO (FineHeap Int)
    mapM_ (heapPut h) xs
    replicateM l (heapPop h)
  Test.QuickCheck.Monadic.assert $ sxs == sort xs

testIt :: IO ()
testIt = quickCheck testSorting

