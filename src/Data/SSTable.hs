{-# LANGUAGE ScopedTypeVariables #-}

-- TODO: currently there is no sampling of keys.

module Data.SSTable
  ( openWriter
  , closeWriter
  , writeEntry
  ) where

import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as L
import qualified System.IO as SysIO
import System.IO (openFile, hSeek, hClose, SeekMode(..), IOMode(..))
import System.Directory (removeFile)
import Control.Monad (when)
import Data.Binary (Binary, encode)
import Data.IORef
import Data.Int (Int32, Int64)
import Text.Printf (printf)

data Writer = Writer 
  { offset  :: IORef Int32
  , datH    :: SysIO.Handle
  , idxH    :: SysIO.Handle 
  , lastKey :: B.ByteString
  , path    :: String }

indexPath path = printf "%s.idx" path

putEncoded :: (Binary a) => SysIO.Handle -> a -> IO ()
putEncoded h = L.hPut h . encode

openWriter :: String -> IO Writer
openWriter path = do
  offRef <- newIORef 8
  dat    <- openFile path WriteMode
  idx    <- openFile (indexPath path) ReadWriteMode

  -- Leave space for the index offset.
  putEncoded dat (0 :: Int64)

  return $ Writer {
      offset  = offRef
    , datH    = dat 
    , idxH    = idx
    , lastKey = B.empty
    , path    = path
  }

closeWriter :: Writer -> IO ()
closeWriter (Writer offRef dat idx _ path) = do
  -- Write out the index offset.
  hSeek dat AbsoluteSeek 0
  offset <- readIORef offRef
  let offset64 :: Int64 = fromIntegral offset
  putEncoded dat offset64

  -- Copy the index over.
  hSeek dat SeekFromEnd 0
  hSeek idx AbsoluteSeek 0
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
writeEntry (Writer offRef dat idx _ _) (key, value) = do
  off <- readIORef offRef
  putEncoded dat valueLen
  B.hPut dat value

  putEncoded idx keyLen
  B.hPut idx key
  L.hPut idx $ encode off

  writeIORef offRef $ off + 4 + (fromIntegral $ valueLen)

  where
    valueLen = B.length value
    keyLen   = B.length key
