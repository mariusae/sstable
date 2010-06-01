{-# LANGUAGE ScopedTypeVariables #-}
module PackingTest where

import Test.QuickCheck
import Test.QuickCheck.Gen
import Test.QuickCheck.Arbitrary

import Data.Bits
import Data.Word
import Data.Int
import Data.SSTable.Packing

instance Arbitrary Word32 where
  arbitrary = (arbitrary :: Gen Integer) >>= return . fromIntegral

instance Arbitrary Word64 where
  arbitrary = do 
    -- Generate from low & high word32s so we make sure we cover
    -- enough bits to test meaningfully.
    low  :: Word32 <- arbitrary
    high :: Word32 <- arbitrary
    return $ (fromIntegral high) `shiftL` 32 .|. (fromIntegral low)

prop_pack32_id :: Word32 -> Bool 
prop_pack32_id w = (unpack32 . pack32) w == w

prop_pack64_id :: Word64 -> Bool 
prop_pack64_id w = (unpack64 . pack64) w == w

