{-# LANGUAGE ScopedTypeVariables #-}

-- TODO: currently there is no sampling of keys.

module Data.SSTable.Writer
  ( openWriter
  , closeWriter
  , writeEntry
  ) where

import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as L
import System.IO (openFile, hSeek, hClose, Handle, SeekMode(..), IOMode(..))
import System.Directory (removeFile)
import Control.Monad (when)
import Data.Binary (Binary, encode)
import Data.IORef
import Data.Int (Int32, Int64)
import Text.Printf (printf)

import Data.SSTable.Encoding

data Writer = Writer 
  { offset  :: IORef Int32
  , count   :: IORef Int32
  , datH    :: Handle
  , idxH    :: Handle 
  , lastKey :: B.ByteString
  , path    :: String }

-- Path to use for temporary index.
indexPath path = printf "%s.idx" path

openWriter :: String -> IO Writer
openWriter path = do
  offRef <- newIORef 8
  cntRef <- newIORef 0
  dat    <- openFile path WriteMode
  idx    <- openFile (indexPath path) ReadWriteMode

  -- Leave space for the index offset.
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
  hSeek dat AbsoluteSeek 0
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
      when (not $ B.null buf) $ B.hPut toH buf >> copy fromH toH

writeEntry :: Writer -> (B.ByteString, B.ByteString) -> IO ()
writeEntry (Writer offRef cntRef dat idx _ _) (key, value) = do
  off <- readIORef offRef
  hPut32 dat $ fromIntegral valueLen
  B.hPut dat value

  hPut32 idx $ fromIntegral keyLen
  B.hPut idx key
  L.hPut idx $ encode off

  writeIORef offRef $ off + 4 + (fromIntegral $ valueLen)
  modifyIORef cntRef (+ 1)

  where
    valueLen = B.length value
    keyLen   = B.length key
