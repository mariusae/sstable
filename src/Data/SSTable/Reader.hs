module Data.SSTable.Reader
  ( openReader
  , closeReader
  , withReader
  , scan
  , leastUpperBound
  ) where

import qualified Data.ByteString as B
import System.IO (openFile, hSeek, hClose, Handle, SeekMode(..), IOMode(..))
import System.IO.Unsafe (unsafeInterleaveIO)
import Control.Monad (when, liftM)
import Data.Array (Array, Ix, bounds, (!))
import Data.Array.IO
import Data.Int
import Text.Printf (printf)

import qualified Data.SSTable
import Data.SSTable (Index, IndexEntry)
import Data.SSTable.Packing

type IOIndex = IOArray Int IndexEntry

data Reader = Reader
  { handle :: Handle 
  , index  :: Index }

openReader :: String -> IO Reader
openReader path = do
  h <- openFile path ReadMode

  version <- hGet32 h
  when (version /= Data.SSTable.version) $
    error $ printf "mismatched versions (expecting %d, got %d)" 
                   Data.SSTable.version version

  -- Fetch the index.
  numBlocks   <- hGet32 h >>= return . fromIntegral
  indexOffset <- hGet64 h >>= return . fromIntegral
  hSeek h AbsoluteSeek indexOffset

  -- Read the index as an array.
  index <- newArray_ (1, numBlocks) :: IO IOIndex

  copy 1 (numBlocks + 1) h index

  -- We don't want to copy the whole index once more, hence the unsafe
  -- freeze.
  liftM (Reader h) $ unsafeFreeze index

  where
    copy i n h index
      | i == n = return ()
      | otherwise = do
          keyLen   <- hGet32 h
          off      <- hGet64 h
          blockLen <- hGet32 h
          key      <- B.hGet h (fromIntegral keyLen)
          writeArray index i (key, fromIntegral off, fromIntegral blockLen)
          copy (i + 1) n h index

closeReader :: Reader -> IO ()
closeReader (Reader h _) = hClose h

withReader :: String -> (Reader -> IO a) -> IO a
withReader path f = do
  reader <- openReader path
  a <- f reader
  closeReader reader
  return a

scan :: Reader -> B.ByteString -> IO [(B.ByteString, B.ByteString)]
scan (Reader h index) begin =
  case leastUpperBound index begin of
    Just n ->  scan' n
    Nothing -> return []

  where
    scan' n 
      | n > upperIndexBound = return []
      | otherwise = do
        block <- fetch n
        scanBlock n block

    -- TODO: this might be nicer with continuations.
    scanBlock n block
      | B.null block = scan' $ n + 1
      | otherwise = do
        let (header, rest)               = B.splitAt 8 block
            (keyLenBytes, entryLenBytes) = B.splitAt 4 header
            keyLen                       = unpackInt keyLenBytes
            entryLen                     = unpackInt entryLenBytes
            (bytes, block')              = B.splitAt (keyLen + entryLen) rest
            (key, entry)                 = B.splitAt keyLen bytes

        if key < begin  -- we may need to skip initial entries
           then scanBlock n block'
           else do 
             next <- unsafeInterleaveIO $ scanBlock n block'
             return $ (key, entry):next

    fetch n = do
      -- (a bounds check has already been performed here)
      let (_, off, len) = index!n
      hSeek h AbsoluteSeek $ fromIntegral off
      B.hGet h $ fromIntegral len

    (_, upperIndexBound) = bounds index

    unpackInt = fromIntegral . unpack32 . B.unpack

-- | /O(log n) in index size./ Find the smallest value in the index
-- greater than or equal to the given key using binary search. We
-- don't specialize the type to 'Index' in order to retain
-- compatibility with our quickcheck tests.
leastUpperBound :: Index -> B.ByteString -> Maybe Int
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
          (this, off, _) = index!mid
