module Data.SSTable
  ( version
  , IndexEntry
  , Index
  ) where

import Data.Word

import qualified Data.ByteString as B
import Data.Int
import Data.Array (Array)

type IndexEntry = (B.ByteString, Int64, Int32)
type Index      = Array Int IndexEntry

version :: Word32
version = 2
