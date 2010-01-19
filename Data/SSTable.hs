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
  withFile path WriteMode $ \handle -> do
    (index, n) <- go handle records [] 0

    -- Store the index at the end, and after it (the last 8 bytes), we
    -- store the index offset.
    indexOffset <- hFileSize handle
    put handle (listArray (0, (length index) - 1) index :: Index)
    put handle (fromIntegral indexOffset :: Int64)

    return ()

  where
    put h x = L.hPut h $ encode x  -- monomorphism :-(

    -- the meat: write records & build the index.
    go _      []          index n = return (reverse index, n)
    go handle ((k, v):rs) index n =
      L.hPut handle encoded >> go handle rs index' n'
      where
        encoded = encode v
        l       = L.length encoded
        n'      = n + l
        index'  = (k, n, fromIntegral l):index

read :: String -> IO SSTableHandle
read path = do
  handle <- openFile path ReadMode
  fs <- hFileSize handle

  -- Read the index offset, and then fetch the entire index.
  hSeek handle AbsoluteSeek $ fs - 8
  (indexOffset::Int64) <- liftM decode $ L.hGet handle 8
  hSeek handle AbsoluteSeek $ fromIntegral indexOffset
  let indexSize = fs - (fromIntegral indexOffset) - 8
  index <- liftM decode $ L.hGet handle (fromIntegral indexSize)
  return $ rnf index  -- normalize (index will be in memory after
                      -- this)

  return $ SSTableHandle handle index

close :: SSTableHandle -> IO ()
close (SSTableHandle h _) = hClose h

-- TODO: allow range queries (this is essential, in fact!)
query (SSTableHandle handle arr) lowerBound key =
  case binarySearch arr key of
    Right offlen -> fetch offlen
    Left  offlen -> if lowerBound then fetch offlen else return Nothing
  where
    fetch (off, len) = do
      hSeek handle AbsoluteSeek $ (fromIntegral off)
      (liftM decode $ L.hGet handle (fromIntegral len)) >>= return . Just

binarySearch arr key = search (minB, maxB)
  where
    (minB, maxB) = bounds arr

    search (lower, upper)
      | key == val                = Right (off, len)
      | key < val && mid /= lower = search (lower, mid - 1)
      | key > val && mid /= upper = search (mid + 1, upper)
      | key < val                 = Left (off', len')
      | otherwise                 = Left (off, len)
        where
          mid = lower + (upper - lower) `div` 2
          (val, off, len) = arr!mid
          (_, off', len') = arr!(max minB (mid - 1))
