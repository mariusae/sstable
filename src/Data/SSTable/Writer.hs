{-# LANGUAGE ScopedTypeVariables #-}

-- The data layout is described in the README.md accompanying this
-- project.

module Data.SSTable.Writer
  ( openWriter
  , closeWriter
  , withWriter
  , writeEntry
  ) where

import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as L
import System.IO (openFile, hSeek, hClose, Handle, SeekMode(..), IOMode(..))
import Control.Monad (unless, when, forM_, liftM)
import Data.Binary (Binary, encode)
import Data.IORef
import Data.Int (Int32, Int64)
import Data.Word (Word32, Word64)
import Text.Printf (printf)

import qualified Data.SSTable
import Data.SSTable.Packing

data Writer = Writer 
  { offset    :: IORef Int64
  , blockLeft :: IORef Int32
  , index     :: IORef [IndexEntry]
  , handle    :: Handle
  , lastKey   :: IORef B.ByteString }

data IndexEntry = IndexEntry
  { key         :: B.ByteString
  , blockOffset :: Int64
  , blockLength :: Int32 }

blockSize = 64 * 1024

openWriter :: String -> IO Writer
openWriter path = do
  offset    <- newIORef $ 4{-VERSION-} + 4{-NUM-ENTRIES-} + 8{-INDEX-OFFSET-}
  blockLeft <- newIORef 0
  index     <- newIORef []
  lastKey   <- newIORef B.empty
  h         <- openFile path WriteMode

  -- Write out the header, filling in for values we do not yet have.
  hPut32 h Data.SSTable.version  -- VERSION
  hPut32 h 0                     -- NUM-ENTRIES    [ to be filled ]
  hPut64 h 0                     -- INDEX-OFFSET   [ to be filled ]

  return $ Writer {
      offset    = offset
    , blockLeft = blockLeft
    , index     = index
    , handle    = h
    , lastKey   = lastKey
  }

closeWriter :: Writer -> IO ()
closeWriter w@(Writer offset _ index h _) = do
  -- Ensure that the last block length is always filled in.
  fillLastBlockLen w

  index' <- liftM reverse $ readIORef index 
  when (null index') $ error "no entries were defined"

  -- Write out the block count & index offset.
  hSeek h AbsoluteSeek 4
  hPut32 h $ fromIntegral $ length index'
  readIORef offset >>= hPut64 h . fromIntegral

  -- Write out the index at the end.
  hSeek h SeekFromEnd 0
  forM_ index' $ \(IndexEntry key blockOffset blockLength) -> do
    hPut32 h $ fromIntegral $ B.length key
    hPut64 h $ fromIntegral blockOffset
    hPut32 h $ fromIntegral blockLength
    B.hPut h key

  hClose h

fillLastBlockLen :: Writer -> IO ()
fillLastBlockLen (Writer _ blockLeft index _ _) = do
  blockLeftVal <- readIORef blockLeft
  modifyIORef index $ \index ->
    case index of
      e : es -> e { blockLength = blockSize - blockLeftVal } : es
      [] -> []

writeEntry :: Writer -> (B.ByteString, B.ByteString) -> IO ()
writeEntry w@(Writer offset blockLeft index h lastKey) (key, value) = do
  keyGreaterThanLast <- readIORef lastKey >>= return . (> key)
  when keyGreaterThanLast $ error "entry keys out of order"

  newBlock <- readIORef blockLeft >>= return . (<= 0)
  when newBlock $ do
    -- Write an index entry with a placeholder length value (we'll
    -- fill that in).  It's a new block!
    fillLastBlockLen w

    blockOffset <- readIORef offset
    let entry = IndexEntry key blockOffset 0
    modifyIORef index ((:) entry)
    writeIORef blockLeft blockSize

  B.hPut h keyLen32
  B.hPut h valueLen32  
  B.hPut h key
  B.hPut h value

  writeIORef  lastKey   key
  modifyIORef blockLeft (+ (- entryLen))
  modifyIORef offset    (+ fromIntegral entryLen)

  where
    keyLen32   = B.pack $ pack32 $ fromIntegral keyLen
    valueLen32 = B.pack $ pack32 $ fromIntegral valueLen
    entryLen   = 4 + 4 + (fromIntegral keyLen) + (fromIntegral valueLen)
    valueLen   = B.length value
    keyLen     = B.length key

withWriter :: String -> (Writer -> IO a) -> IO a
withWriter path f = do
  writer <- openWriter path
  a <- f writer
  closeWriter writer
  return a
  