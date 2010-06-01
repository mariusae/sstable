module Main where

import Test.Framework (defaultMain, testGroup)
import Test.Framework.Providers.QuickCheck2 (testProperty)

import PackingTest
import BinarySearchTest

main = defaultMain tests
  where
    tests = [packing, binarySearch]
    packing = testGroup "Packing"
      [ testProperty "32-bit packing" prop_pack32_id
      , testProperty "64-bit packing" prop_pack64_id ]

    binarySearch = testGroup "Binary search"
      [ testProperty "querying" prop_binary_search_works
      , testProperty "arbitrary lookups" prop_arbitrary_lookups_work
      , testProperty "querying outside range" prop_querying_outside_range_works
      , testProperty "ensure boundedness" prop_ensure_boundedness ]
