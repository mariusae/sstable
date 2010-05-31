module Data.SSTable.Iteratee
  (
  ) where

-- An iteratee based interface for writing sstables.  Can iteratees in
-- turn be enumerators?  If so, then this is an ideal way to do
-- transformations over SSTables, etc. (and indeed to write the
-- resulting sstable somewhere).
--
-- We should then have an interface that turns a list into an
-- enumerator.

import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import qualified Data.Iteratee.Base as Iter
import Data.Iteratee (IterateeG(..))

import Debug.Trace

entryIter :: IterateeG [] (ByteString, ByteString) IO ()
entryIter = Iter.foldl f ()
  where
    f x entry = trace (show entry) ()
  