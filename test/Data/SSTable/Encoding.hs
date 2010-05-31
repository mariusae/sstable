import Test.QuickCheck
import Test.QuickCheck.Gen
import Test.QuickCheck.Arbitrary

import Data.Word
import Data.Int
import Data.SSTable.Encoding

instance Arbitrary Word32 where
  arbitrary = (arbitrary :: Gen Integer) >>= return . fromIntegral

instance Arbitrary Word64 where
  arbitrary = (arbitrary :: Gen Integer) >>= return . fromIntegral

prop_pack32_id :: Word32 -> Bool 
prop_pack32_id w = (unpack32 . pack32) w == w

prop_pack64_id :: Word64 -> Bool 
prop_pack64_id w = (unpack64 . pack64) w == w

main = do
  check prop_pack32_id
  check prop_pack64_id
  where
    check t = quickCheckWith (stdArgs { maxSize = 200, maxSuccess = 500 }) t