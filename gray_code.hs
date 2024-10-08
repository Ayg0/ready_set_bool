module GrayCode where
import Data.Word (Word32)
import Data.Bits (shiftL, xor, testBit, (.&.), (.|.))


gray_code :: Word32 -> Word32
getXorResult :: Word32 -> Int -> Word32

----------------------------------------------------------------
getXorResult nbr pos = (if xorRes then 1 else 0) `shiftL` pos
 where
  xorRes =  (testBit nbr pos) /= testBit nbr (pos + 1)

----------------------------------------------------------------
gray_code nbr = aux nbr 0 0
 where
  aux nbr pos res
   | pos == 31 = (nbr .&. (1 `shiftL` fromIntegral pos)) .|. res
   | otherwise = aux nbr (pos + 1) accumulator
    where 
     accumulator = res .|. getXorResult nbr pos
