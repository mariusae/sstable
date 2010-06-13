{-# LANGUAGE FlexibleInstances #-}

module BinarySearchTest where

import Test.QuickCheck
import Test.QuickCheck.Gen
import Test.QuickCheck.Arbitrary

import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as C8
import Data.List
import Data.Array
import Data.SSTable.Reader
import Data.Maybe
import Data.Int

import System.IO.Unsafe

import Debug.Trace
import Text.Printf

-- Create a newtype so we can have our own arbitrary instance.
newtype IndexList
 = IA { unIA :: [(B.ByteString, Int64, Int32)] }
   deriving (Show)

instance Arbitrary B.ByteString where
  arbitrary = arbitrary >>= return . C8.pack

instance Arbitrary Int64 where
  arbitrary = (arbitrary :: Gen Integer) >>= return . fromIntegral

instance Arbitrary Int32 where
  arbitrary = (arbitrary :: Gen Integer) >>= return . fromIntegral

instance Arbitrary IndexList where
  -- Creates an indexlist of sizes between 5 and 200.
  arbitrary = arbitrary >>= return . min 200 . max 5 >>= vector >>= return . IA

fst3 (x, _, _) = x

-- Filter out adjacent keys.
makeSearchList l = nubBy (\(a, _, _) (b, _, _) -> a == b) (sort l)
toArray l        = listArray (0, (length l) - 1) l
makeSearchIndex  = toArray . makeSearchList

-- Make sure we find each item.
prop_binary_search_works :: IndexList -> Bool
prop_binary_search_works (IA l) = go ((snd . bounds) index)
  where
    index = makeSearchIndex l

    go (-1) = True
    go n = leastUpperBound index (fst3 $ index!n) == Just n && (go $ n - 1)

prop_arbitrary_lookups_work :: IndexList -> B.ByteString -> Bool
prop_arbitrary_lookups_work (IA l) key =
  case leastUpperBound index key of
    Just n  -> key <= (fst3 $ index!n)
    Nothing ->
      let (l, u) = bounds index in
      l > u || key > (fst3 $ index!u)
  where
    index = makeSearchIndex l

prop_querying_outside_range_works :: IndexList -> B.ByteString -> Property
prop_querying_outside_range_works (IA l) key =
  len > 0 ==>
    -- strings longer by suffix are always bigger (at least in
    -- whatever collation i'm using(!))
    leastUpperBound index (B.concat [biggestKey, C8.singleton 'a']) == Nothing
  where
    l' = makeSearchList l
    index = toArray l'
    len = length l'
    (biggestKey, _, _) = index!(len - 1)

prop_ensure_boundedness :: IndexList -> Int -> Property
prop_ensure_boundedness (IA l) which =
  length l' > 2 ==>
    leastUpperBound index pickVal == Just pick
  where
    l'              = makeSearchList l
    pick            = (abs which) `mod` (((length l') - 2) + 1)
    l''             = take pick l' ++ drop (pick + 1) l'
    index           = toArray l''
    (pickVal, _, _) = l'!!pick
