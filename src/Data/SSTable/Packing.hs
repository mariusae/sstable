module Data.SSTable.Packing
  ( -- * big-endian packs.
    pack32
  , pack64
  , unpack32
  , unpack64

    -- ** convenience functions for @Handle@s
  , hPut32
  , hPut64
  , hGet32
  , hGet64
  ) where

import Data.Bits
import Data.Word
import System.IO (Handle)
import qualified Data.ByteString as B

pack32 :: Word32 -> [Word8]
pack32 w =
  [ fromIntegral $ w `shiftR` 24 :: Word8
  , fromIntegral $ w `shiftR` 16 :: Word8
  , fromIntegral $ w `shiftR`  8 :: Word8
  , fromIntegral $ w             :: Word8 ]

pack64 :: Word64 -> [Word8]
pack64 w =
  pack32 high ++ pack32 low
  where
    high = fromIntegral $ w `shiftR` 32 :: Word32
    low  = fromIntegral $ w             :: Word32

unpack32 :: [Word8] -> Word32
unpack32 ws =
      w0 `shiftL` 24 
  .|. w1 `shiftL` 16
  .|. w2 `shiftL` 8
  .|. w3
  where
    w0 : w1 : w2 : w3 : [] = map fromIntegral $ take 4 ws
    
unpack64 :: [Word8] -> Word64
unpack64 ws =
  high `shiftL` 32 .|. low
  where
    high = fromIntegral . unpack32 $ take 4 ws
    low  = fromIntegral . unpack32 $ take 4 . drop 4 $ ws

hPut32 :: Handle -> Word32 -> IO ()
hPut32 h w = B.hPut h bytes
  where
    bytes = B.pack (pack32 w)

hPut64 :: Handle -> Word64 -> IO ()
hPut64 h w = B.hPut h bytes
  where
    bytes = B.pack (pack64 w)

hGet32 :: Handle -> IO Word32
hGet32 h = B.hGet h 4 >>= return . unpack32 . B.unpack

hGet64 :: Handle -> IO Word64
hGet64 h = B.hGet h 8 >>= return . unpack64 . B.unpack
