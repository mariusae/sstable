{-# LANGUAGE ScopedTypeVariables #-}

-- The data layout is:
--   version     :: Word32    | header
--   indexOffset :: Word64
--   entry0Len   :: Word32    | entries..
--   entry0Bytes :: [Word8]
--   entry1Len   :: Word32
--   entry1Bytes :: [Word8]
--   ...
--   entryCount  :: Word32    | index header
--   key0Off     :: Word64    | index..
--   key0Len     :: Word32
--   key0Bytes   :: [Word8]
--   key1Off     :: Word64
--   key1Len     :: Word32
--   key1Bytes   :: [Word8]
-- 
-- There is currently no key sampling, and we always load the index
-- entirely into memory.

module Data.SSTable.Writer
  ( openWriter
  , closeWriter
  , withWriter
  , writeEntry
  ) where

import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as L
import System.IO (openFile, hSeek, hClose, Handle, SeekMode(..), IOMode(..))
import System.Directory (removeFile)
import Control.Monad (unless)
import Data.Binary (Binary, encode)
import Data.IORef
import Data.Int (Int32, Int64)
import Text.Printf (printf)

import qualified Data.SSTable
import Data.SSTable.Packing

data Writer = Writer 
  { offset  :: IORef Int64
  , count   :: IORef Int32
  , datH    :: Handle
  , idxH    :: Handle 
  , lastKey :: B.ByteString
  , path    :: String }

-- Path to use for temporary index.
indexPath path = printf "%s.idx" path

openWriter :: String -> IO Writer
openWriter path = do
  offRef <- newIORef (4{-version-} + 8{-index offset-})
  cntRef <- newIORef 0
  dat    <- openFile path WriteMode
  idx    <- openFile (indexPath path) ReadWriteMode

  -- Write the version, and leave space for the index offset.
  hPut32 dat Data.SSTable.version
  hPut64 dat 0

  return $ Writer {
      offset  = offRef
    , count   = cntRef
    , datH    = dat
    , idxH    = idx
    , lastKey = B.empty
    , path    = path
  }

closeWriter :: Writer -> IO ()
closeWriter (Writer offRef cntRef dat idx _ path) = do
  -- Write out the index offset.
  hSeek dat AbsoluteSeek 4
  readIORef offRef >>= hPut64 dat . fromIntegral

  -- Copy the index over, but first the count.
  hSeek dat SeekFromEnd 0
  hSeek idx AbsoluteSeek 0
  readIORef cntRef >>= hPut32 dat . fromIntegral
  copy idx dat

  hClose dat
  hClose idx

  removeFile $ indexPath path

  where
    bufSz = 1024 * 1024
    copy fromH toH = do
      buf <- B.hGet fromH bufSz
      unless (B.null buf) $ B.hPut toH buf >> copy fromH toH

writeEntry :: Writer -> (B.ByteString, B.ByteString) -> IO ()
writeEntry (Writer offRef cntRef dat idx _ _) (key, value) = do
  hPut32 dat $ fromIntegral valueLen
  B.hPut dat value

  off <- readIORef offRef
  hPut64 idx $ fromIntegral off
  hPut32 idx $ fromIntegral keyLen

  B.hPut idx key

  modifyIORef offRef (+ (4 + (fromIntegral valueLen)))
  modifyIORef cntRef (+ 1)

  where
    valueLen = B.length value
    keyLen   = B.length key

withWriter :: String -> (Writer -> IO a) -> IO a
withWriter path f = do
  writer <- openWriter path
  a <- f writer
  closeWriter writer
  return a
  