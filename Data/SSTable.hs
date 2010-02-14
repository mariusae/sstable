{-# LANGUAGE ScopedTypeVariables, BangPatterns #-}
module Data.SSTable
  ( SSTableHandle
  , write
  , read
  , close
  , query

  -- exported for unittesting
  , leastUpperBound
  ) where

import Prelude hiding (read)
import Control.Monad (foldM, forM_, liftM, liftM2)
import Control.DeepSeq (rnf)
import Data.Binary (Binary, encode, decode)
import Data.Int (Int32, Int64)
import Data.Map (Map)
import Data.Array (Array, listArray, bounds, (!), Ix)
import qualified Data.Map as Map
import qualified Data.ByteString.Lazy as L
import System.IO (
  Handle, hSeek, hIsEOF, hFileSize, hClose, hFlush,
  SeekMode(AbsoluteSeek), withFile, openFile,
  IOMode(WriteMode, ReadMode),
  BufferMode(BlockBuffering), hSetBuffering)

import System.IO.Unsafe (unsafeInterleaveIO)
import Debug.Trace
import Text.Printf

type Index =
  Array Int ( String  -- key
            , Int64   -- file offset
            , Int32   -- length
            )

data SSTableHandle
  = SSTableHandle Handle Index
    deriving (Show)

foldM' a xs f = foldM f a xs

write :: (Binary a) => String -> [(String, a)] -> IO ()
write path records = do
  withFile path WriteMode $ \fh -> do
    hSetBuffering fh $ BlockBuffering $ Just 134217728

    (_, index) <- 
      foldM' (0, []) records $ \(n, index) (k, v) -> do
        let !encoded = encode v
            l        = L.length encoded
            !this    = (k, n, fromIntegral l)
        L.hPut fh encoded
        let !x = rnf this `seq` this:index
        return (n + l, x)

    -- Store the index at the end, and after it (the last 8 bytes), we
    -- store the index offset.
    indexOffset <- hFileSize fh
    put fh (listArray (0, (length index) - 1) (reverse index) :: Index)
    put fh (fromIntegral indexOffset :: Int64)

    hFlush fh
    return ()

  where
    put h x = L.hPut h $ encode x  -- monomorphism :-(

read :: String -> IO SSTableHandle
read path = do
  fh <- openFile path ReadMode
  fs <- hFileSize fh

  -- Read the index offset, and then fetch the entire index.
  hSeek fh AbsoluteSeek $ fs - 8
  (indexOffset::Int64) <- liftM decode $ L.hGet fh 8
  hSeek fh AbsoluteSeek $ fromIntegral indexOffset
  let indexSize = fs - (fromIntegral indexOffset) - 8
  index <- liftM decode $ L.hGet fh (fromIntegral indexSize)

  -- normalize (index will be in memory after this) before packing it
  -- in a handle
  let !_ = rnf index
  return $ SSTableHandle fh index

close :: SSTableHandle -> IO ()
close (SSTableHandle fh _) = hClose fh

scan (SSTableHandle fh index) n = do
  scan' n
  where
    (_, upperBound) = bounds index

    scan' n
      | n > upperBound = return []
      | otherwise = do
          let (key, off, len) = index!n
          entry <- fetch off len
          next <- unsafeInterleaveIO $ scan' (n + 1)
          return $ (key, entry):next

    fetch off len = do
      -- seek every time, so that we can interleave IO safely.
      hSeek fh AbsoluteSeek $ (fromIntegral off)
      liftM decode $ L.hGet fh (fromIntegral len)

query h@(SSTableHandle _ index) begin = do
  case leastUpperBound index begin of
    Nothing -> return []
    Just n  -> scan h n

-- | /O(log n) in index size./ Find the smallest value in the index
-- greater than or equal to the given key using binary search. We
-- don't specialize the type to 'Index' in order to retain
-- compatibility with our quickcheck tests.
leastUpperBound
  :: (Ix i, Ord a, Integral i) => Array i (a, t, t1) -> a -> Maybe i
leastUpperBound index key =
  if lowerBound > upperBound
     then Nothing
     else go indexBounds
  where
    indexBounds@(lowerBound, upperBound) = bounds index

    go (lower, upper)
      -- Standard binary search. Keep in mind the bounds are
      -- inclusive.
      | key == this = Just mid
      | key < this && mid /= lower = go (lower, mid - 1)
      | key > this && mid /= upper = go (mid + 1, upper)

      -- We failed to descend (and thus to find our exact
      -- value). `lower' and `upper' are individually either a bounds
      -- on our search key, or they are the extremes of the index
      -- values.
      --
      -- Thus to find our least upper bound:
      --
      --   If the key is greater than our current value and we are not
      --   at the table extreme, choose our current upper bound.
      | key > this && mid /= upperBound = Just $ mid + 1

      --   If the key is smaller than our current value, select our
      --   current value (the next value down either does not exist or
      --   is smaller than our key by implication of the bounds)
      | key < this = Just mid

      -- Otherwise we fail our search. This happens if the largest
      -- value in the table is smaller than our search key.
      | otherwise = Nothing
        where
          mid = lower + (upper - lower) `div` 2
          (this, off, len) = index!mid
