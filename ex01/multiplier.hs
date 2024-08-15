import Adder (adder) 
import Data.Word (Word32)
import Data.Bits (complement)

main:: IO()
multiplier :: Word32 -> Word32 -> Word32
decrement:: Word32 -> Word32

----------------------------------------------------------------
decrement nbr = adder nbr ((complement 1) + 1)

----------------------------------------------------------------
multiplier nbr1 nbr2 = aux nbr1 nbr2 0
 where
  aux nbr1 nbr2 res
   | nbr2 == 0 = res
   | otherwise = aux nbr1 (decrement nbr2) (adder res nbr1)

main = print(multiplier 55 2)