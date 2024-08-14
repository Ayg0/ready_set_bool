module Adder where
import Data.Word (Word32)
import Data.Bits (shiftL, xor, testBit, (.&.), (.|.))

main:: IO()
adder :: Word32 -> Word32 -> Word32
halfAdder :: Bool -> Bool -> (Bool, Bool)
fullAdder :: Bool -> Bool -> Bool -> (Bool, Bool)
fullAdderPos :: Word32 -> Word32 -> Bool -> Int -> (Bool, Bool)

----------------------------------------------------------------
halfAdder bit1 bit2 = (sum, carry)
 where
  sum = bit1 `xor` bit2
  carry = bit1 .&. bit2

----------------------------------------------------------------
fullAdder bit1 bit2 prevCarry = (sum, nextCarry)
 where
  (halfAdderSum, halfAdderCarry) = halfAdder bit1 bit2
  sum = halfAdderSum `xor` prevCarry
  nextCarry = (halfAdderSum .&. prevCarry) .|. (bit1 .&. bit2)

----------------------------------------------------------------
fullAdderPos nbr1 nbr2 prevCarry pos = 
 let
  bit1 = testBit nbr1 pos
  bit2 = testBit nbr2 pos
 in
  fullAdder bit1 bit2 prevCarry

----------------------------------------------------------------
adder nbr1 nbr2 = aux nbr1 nbr2 0 0 False
 where
  aux nbr1 nbr2 pos res prevCarry
   | pos == 32 = res
   | otherwise = aux nbr1 nbr2 (pos + 1) currentRes currentCarry
    where
     (sum, currentCarry) = fullAdderPos nbr1 nbr2 prevCarry pos
     sumWord32 = if sum == True then 1 `shiftL` pos else 0
     currentRes = sumWord32 .|. res

main =
 do
  print (adder 507 507)
 
