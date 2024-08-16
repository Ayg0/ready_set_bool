module Multiplier where
import Adder (adder)
import Sub (sub)
import Data.Word (Word32)

multiplier :: Word32 -> Word32 -> Word32
decrement:: Word32 -> Word32

----------------------------------------------------------------
decrement nbr = sub nbr 1

----------------------------------------------------------------
multiplier nbr1 nbr2 = aux nbr1 nbr2 0
 where
  aux nbr1 nbr2 res
   | nbr2 == 0 = res
   | otherwise = aux nbr1 (decrement nbr2) (adder res nbr1)
