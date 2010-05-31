module Data.SSTable.Reader
  (
  ) where

import qualified Data.ByteString as B
import System.IO (openFile, hSeek, hClose, Handle, SeekMode(..), IOMode(..))

type Index = 
  Array Int ( B.ByteString  -- key
            , Int64         -- file offset
            , Int32         -- length
            )

data Reader = Reader
  { handle :: Handle 
  , index  :: Index }

getEncoded :: (Binary a) => Handle -> IO a
getEncoded h = L.hPut h . encode

openReader :: String -> IO Reader
openReader path = do
  h <- openFile path ReadMode

  -- Fetch the index.

  return $ Reader { handle = h }

closeReader :: Reader -> IO ()
closeReader (Reader h) = do
  hClose h

