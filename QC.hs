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

newtype IndexList
 = IA { unIA :: [(String, Int, Int)] }
   deriving (Show)

instance Arbitrary IndexList where
  arbitrary = arbitrary >>= return . min 200 . max 5 >>= vector >>= return . IA

makeSearchList l = nubBy (\(a, _, _) (b, _, _) -> a == b) (sort l)
toArray l = listArray (0, (length l) - 1) l
makeSearchArray = toArray . makeSearchList

prop_binary_search_works :: IndexList -> Bool
prop_binary_search_works (IA l) = go ((snd . bounds) arr)
  where
    arr = makeSearchArray l
    split n = (a, (b, c)) where (a, b, c) = arr!n

    go (-1) = True
    go n = binarySearch arr key == Just n && (go $ n - 1)
      where
        (key, val) = split $ n

fst3 (x, _, _) = x

prop_binary_search_negative_lookup_works :: IndexList -> Property
prop_binary_search_negative_lookup_works (IA l) =
  length l' > 4 ==>
    isJust $ binarySearch arr firstElem
  where
    -- FIXME ugh. the following is *hideous* :-/
    l'        = makeSearchList l
    firstElem = (fst3 . head) l'
    lastElem  = last l'
    mid       = length l' `div` 2
    midElem   = l'!!mid
    (a, b)    = splitAt mid l'
    firstHalf = drop 1 a
    lastHalf  = (take (mid - 1) . drop 1) a
    arr       = toArray (firstHalf ++ lastHalf)



prop_arbitrary_lookups_work :: IndexList -> String -> Bool
prop_arbitrary_lookups_work (IA l) key =
  case binarySearch arr key of
    Just n  -> key <= (fst3 $ arr!n)
    Nothing ->
      let (l, u) = bounds arr in
      l > u || key > (fst3 $ arr!u)
  where
    arr = makeSearchArray l
    lookupList = map (\(key, _, _) -> (key, True)) l

prop_querying_outside_range_works :: IndexList -> String -> Property
prop_querying_outside_range_works (IA l) key =
  len > 0 ==>
    -- strings longer by suffix are always bigger (at least in
    -- whatever collation i'm using(!))
    binarySearch arr (biggestKey ++ "a") == Nothing
  where
    l' = makeSearchList l
    arr = toArray l'
    len = length l'
    (biggestKey, _, _) = arr!(len - 1)

check t = quickCheckWith (stdArgs { maxSize = 200, maxSuccess = 500 }) t

main = do
  check prop_binary_search_works
  check prop_arbitrary_lookups_work
  check prop_querying_outside_range_works