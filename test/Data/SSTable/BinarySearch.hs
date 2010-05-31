{-# LANGUAGE FlexibleInstances #-}

import Test.QuickCheck
import Test.QuickCheck.Gen
import Test.QuickCheck.Arbitrary

import Data.List
import Data.Array
import Data.SSTable
import Data.Maybe

import System.IO.Unsafe

import Debug.Trace
import Text.Printf

-- Create a newtype so we can have our own arbitrary instance.
newtype IndexList
 = IA { unIA :: [(String, Int, Int)] }
   deriving (Show)

instance Arbitrary IndexList where
  -- Creates an indexlist of sizes between 5 and 200.
  arbitrary = arbitrary >>= return . min 200 . max 5 >>= vector >>= return . IA

-- Filter out adjacent keys.
makeSearchList l = nubBy (\(a, _, _) (b, _, _) -> a == b) (sort l)
toArray l        = listArray (0, (length l) - 1) l
makeSearchIndex  = toArray . makeSearchList

fst3 (x, _, _) = x

-- Make sure we find each item.
prop_binary_search_works :: IndexList -> Bool
prop_binary_search_works (IA l) = go ((snd . bounds) index)
  where
    index = makeSearchIndex l

    go (-1) = True
    go n = leastUpperBound index (fst3 $ index!n) == Just n && (go $ n - 1)

prop_arbitrary_lookups_work :: IndexList -> String -> Bool
prop_arbitrary_lookups_work (IA l) key =
  case leastUpperBound index key of
    Just n  -> key <= (fst3 $ index!n)
    Nothing ->
      let (l, u) = bounds index in
      l > u || key > (fst3 $ index!u)
  where
    index = makeSearchIndex l

prop_querying_outside_range_works :: IndexList -> String -> Property
prop_querying_outside_range_works (IA l) key =
  len > 0 ==>
    -- strings longer by suffix are always bigger (at least in
    -- whatever collation i'm using(!))
    leastUpperBound index (biggestKey ++ "a") == Nothing
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

main = do
  check prop_binary_search_works
  check prop_arbitrary_lookups_work
  check prop_querying_outside_range_works
  check prop_ensure_boundedness
  where
    check t = quickCheckWith (stdArgs { maxSize = 200, maxSuccess = 500 }) t
