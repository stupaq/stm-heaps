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

type TheHeap = FineHeap

data FineHeap e = Nil | FineHeap {
  heapSize  :: TVar Int
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

putElemRec :: (Ord e) => FineHeap e -> e -> IO ()
putElemRec (FineHeap _ _ left right) new = do
  next <- atomically $ do
    -- We choose the smaller subtree for insertion, note that we do not
    -- care whether the children have gaps -- we drive our decision based
    -- on size only.
    [lsz, rsz] <- mapM (readTVar . heapSize) [left, right]
    let root = if lsz < rsz then left else right
    -- Update it.
    fmap (root, ) <$> putElem root new
  -- And proceed recursively if necessary.
  case next of
    Nothing -> return ()
    Just (root', new') -> putElemRec root' new'

putGap :: FineHeap e -> STM e
putGap root@(FineHeap size elem left right) = do
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

lowerGap :: (Ord e) => FineHeap e -> IO ()
lowerGap root@(FineHeap _ elem left right) = do
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
      -- TODO prove it by hand
      (_, Just _) ->
        return Nothing
  case next of
    Nothing -> return ()
    Just tree -> lowerGap tree
    where
      replaceGap tree@(FineHeap size' elem' _ _) el = do
        writeTVar elem $ Just el
        writeTVar elem' Nothing
        readTVar size' >>= writeTVar size' . subtract 1
        return $ Just tree

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

  heapPop root@(FineHeap {}) = do
    el <- atomically $ putGap root
    lowerGap root
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
testIt = quickCheckWith (stdArgs { maxSuccess = 10000 }) testSorting

