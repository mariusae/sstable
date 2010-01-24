{-# LANGUAGE ScopedTypeVariables, BangPatterns #-}

module Data.SSTable
  ( write
  , read
  , close
  , query
  , binarySearch
  ) where

import Prelude hiding (read)
import Control.Monad (forM_, liftM, liftM2)
import Control.DeepSeq (rnf)
import Data.Binary (Binary, encode, decode)
import Data.Int (Int32, Int64)
import Data.Map (Map)
import Data.Array (Array, listArray, bounds, (!))
import qualified Data.Map as Map
import qualified Data.ByteString.Lazy as L
import System.IO (
  Handle, hSeek, hIsEOF, hFileSize, hClose,
  SeekMode(AbsoluteSeek), withFile, openFile,
  IOMode(WriteMode, ReadMode))

type Index = Array Int (String, Int64, Int32)

data SSTableHandle
  = SSTableHandle Handle Index
    deriving (Show)

write :: (Binary a) => String -> [(String, a)] -> IO ()
write path records = do
  withFile path WriteMode $ \fh -> do
    (index, n) <- go fh records [] 0

    -- Store the index at the end, and after it (the last 8 bytes), we
    -- store the index offset.
    indexOffset <- hFileSize fh
    put fh (listArray (0, (length index) - 1) index :: Index)
    put fh (fromIntegral indexOffset :: Int64)

    return ()

  where
    put h x = L.hPut h $ encode x  -- monomorphism :-(

    -- the meat: write records & build the index.
    go _      []          index n = return (reverse index, n)
    go fh ((k, v):rs) index n =
      L.hPut fh encoded >> go fh rs index' n'
      where
        encoded = encode v
        l       = L.length encoded
        n'      = n + l
        index'  = (k, n, fromIntegral l):index

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
  return $ rnf index  -- normalize (index will be in memory after
                      -- this)

  return $ SSTableHandle fh index

close :: SSTableHandle -> IO ()
close (SSTableHandle fh _) = hClose fh

query (SSTableHandle fh arr) begin end =
  undefined

-- -- TODO: allow range queries (this is essential, in fact!)
-- query (SSTableHandle fh arr) lowerBound key =
--   case binarySearch arr key of
--     Right offlen -> fetch offlen
--     Left  offlen -> if lowerBound then fetch offlen else return Nothing
--   where
--     fetch (off, len) = do
--       hSeek fh AbsoluteSeek $ (fromIntegral off)
--       (liftM decode $ L.hGet fh (fromIntegral len)) >>= return . Just

binarySearch index key =
  if lowerBound > upperBound
     then Nothing
     else go indexBounds
  where
    indexBounds@(lowerBound, upperBound) = bounds index

    go (lower, upper)
      | key == this = Just mid
      | key < this && mid /= lower = go (lower, mid - 1)
      | key > this && mid /= upper = go (mid + 1, upper)
      -- We failed to descend (and thus to find our exact value).
      --
      -- We also know that `lower' and `upper' are either
      --   1. smaller/bigger than the value we're searching for, or
      --   2. the smallest/biggest value in the table
      --
      -- Thus:
      --   If the key is greater (and we're not at the uppermost
      --   boundary), the next entry is the lower bound in our
      --   table.
      | key > this && mid /= upperBound = Just $ mid + 1
      | key < this = Just mid
      | otherwise = Nothing
        where
          mid = lower + (upper - lower) `div` 2
          (this, off, len) = index!mid
