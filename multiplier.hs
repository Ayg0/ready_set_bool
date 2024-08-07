import Adder (adder) 
import Data.Word (Word32)

main:: IO()
multiplier :: Word32 -> Word32 -> Word32

multiplier nbr1 nbr2 = aux nbr1 nbr2 0
 where
  aux nbr1 nbr2 res
   | nbr2 == 0 = res
   | otherwise = aux nbr1 (nbr2 - 1) (adder res nbr1)

main = print(multiplier 10 4)