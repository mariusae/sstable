{-# LANGUAGE FlexibleInstances #-}

import Test.QuickCheck
import Test.QuickCheck.Gen
import Test.QuickCheck.Arbitrary

import Data.List
import Data.Array
import Data.SSTable

import System.IO.Unsafe

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
    go n = binarySearch arr key == Right val && (go $ n - 1)
      where
        (key, val) = split $ n

isLeft (Left  _) = True
isLeft (Right _) = False

prop_binary_search_negative_lookup_works :: IndexList -> Property
prop_binary_search_negative_lookup_works (IA l) =
  length l' > 4 ==>
    isLeft $ binarySearch arr firstElem
  where
    fst3 (x, _, _) = x

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
    Right _ -> lookup key lookupList /= Nothing
    Left _  -> lookup key lookupList == Nothing
  where
    arr = makeSearchArray l
    lookupList = map (\(key, _, _) -> (key, True)) l

main = do
  quickCheck prop_binary_search_works
  quickCheck prop_binary_search_negative_lookup_works
  quickCheck prop_arbitrary_lookups_work