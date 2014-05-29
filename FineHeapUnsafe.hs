{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TupleSections         #-}
module FineHeapUnsafe (TheHeap, FineHeapUnsafe, testIt, heapPopWithGap, heapUngap) where

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

type TheHeap = FineHeapUnsafe

data FineHeapUnsafe e = Nil | FineHeapUnsafe {
  heapSize  :: TVar Int
, heapElem  :: TVar (Maybe e)
, heapLeft  :: FineHeapUnsafe e
, heapRight :: FineHeapUnsafe e
}

readNode :: FineHeapUnsafe e -> STM (Int, Maybe e)
readNode (FineHeapUnsafe size elem _ _) = liftM2 (,) (readTVar size) (readTVar elem)
readNode Nil = return (0, Nothing)

heapPutElem :: (Ord e) => FineHeapUnsafe e -> e -> STM (Maybe e)
heapPutElem root@(FineHeapUnsafe size elem _ _) new = do
  (sz, el) <- readNode root
  writeTVar size (sz + 1)
  case (sz, el) of
    (0, Nothing) -> do
      -- This is an empty node, we can put our stuff here and finish.
      writeTVar elem $ Just new
      return Nothing
    (_, Nothing) ->
      -- This is a gap, we'll wait for someone to get rid of it.
      retry
    (_, Just el) -> do
      -- We swap the element to be inserted if necessary.
      if new < el
      then writeTVar elem (Just new) >> return (Just el)
      else return (Just new)

heapPutElemRec :: (Ord e) => FineHeapUnsafe e -> e -> IO ()
heapPutElemRec (FineHeapUnsafe _ _ left right) new = do
  next <- atomically $ do
    -- We choose the smaller subtree for insertion, note that we do not
    -- care whether the children have gaps -- we drive our decision based
    -- on size only.
    [lsz, rsz] <- mapM (readTVar . heapSize) [left, right]
    let root = if lsz < rsz then left else right
    -- Update it.
    fmap (root, ) <$> heapPutElem root new
  -- And proceed recursively if necessary.
  case next of
    Nothing -> return ()
    Just (root', new') -> heapPutElemRec root' new'

heapPopWithGap :: FineHeapUnsafe e -> STM e
heapPopWithGap root@(FineHeapUnsafe size elem left right) = do
  (sz, el) <- readNode root
  writeTVar size (sz - 1)
  check $ sz > 0
  case el of
    Nothing ->
      -- This is a gap, we'll wait for someone to push it down.
      retry
    Just el ->
      -- We create a gap, we'll deal with it in a moment.
      writeTVar elem Nothing >> return el

heapUngap :: (Ord e) => FineHeapUnsafe e -> IO ()
heapUngap root@(FineHeapUnsafe _ elem left right) = do
  next <- atomically $ do
    -- The size has been decremented by someone who put a gap here.
    (sz, el) <- readNode root
    case (sz, el) of
      (0, _) ->
        -- If it's empty then we're done.
        return Nothing
      (_, Nothing) -> do
        -- Otherwise choose a min child.
        children <- mapM readNode [left, right]
        case children of
          -- In these cases we have no choice.
          [(0, Nothing), (_, Just rel)] -> replaceGap right rel
          [(_, Just lel), (0, Nothing)] -> replaceGap left lel
          -- Otherwise, we hold back if any of the nodes is a gap.
          [(_, Nothing), _] -> retry
          [_, (_, Nothing)] -> retry
          -- Finally, we swap with min child
          [(lsz, Just lel), (rsz, Just rel)] ->
            let (tree, el) = minimumBy (comparing snd) [(left, lel), (right, rel)]
            in replaceGap tree el
      (_, Just _) ->
        -- This can happen on one element queue, when someone overrides
        -- empty queue left by us with an element.
        return Nothing
  case next of
    Nothing -> return ()
    Just tree -> heapUngap tree
    where
      replaceGap tree@(FineHeapUnsafe size' elem' _ _) el = do
        writeTVar elem $ Just el
        writeTVar elem' Nothing
        readTVar size' >>= writeTVar size' . subtract 1
        return $ Just tree

-- The user must ensure mutual exclusion between put and pop operations.
instance (Ord e) => ConcurrentHeap (FineHeapUnsafe e) e where
  heapNew n
    | n > 0 = do
      s <- newTVarIO 0
      e <- newTVarIO Nothing
      let m = n `quot` 2 -- ceil((n - 1) / 2)
      [tl, tr] <- replicateM 2 $ heapNew m
      return $ FineHeapUnsafe s e tl tr
    | otherwise = return Nil

  heapPut root@(FineHeapUnsafe {}) new = do
    next <- atomically $ heapPutElem root new
    case next of
      Nothing -> return ()
      Just new' -> heapPutElemRec root new'

  heapPop root@(FineHeapUnsafe {}) = do
    el <- atomically $ heapPopWithGap root
    heapUngap root
    return el

testSorting :: [Int] -> Property
testSorting xs = monadicIO $ do
  sxs <- run $ do
    let l = length xs
    h <- heapNew l :: IO (FineHeapUnsafe Int)
    mapM_ (heapPut h) xs
    replicateM l (heapPop h)
  Test.QuickCheck.Monadic.assert $ sxs == sort xs

testIt :: IO ()
testIt = quickCheckWith (stdArgs { maxSuccess = 10000 }) testSorting

