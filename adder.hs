import Data.Word (Word32)
import Data.Bits (shiftR, shift, xor, testBit, (.&.), (.|.))

main:: IO()
printBits :: Integer -> IO ()
adder :: Word32 -> Word32 -> Word32
halfAdder :: Bool -> Bool -> (Bool, Bool)
fullAdder :: Bool -> Bool -> Bool -> (Bool, Bool)
fullAdderPos :: Word32 -> Word32 -> Bool -> Int -> (Bool, Bool)

halfAdder bit1 bit2 = (sum, carry)
 where
  sum = bit1 `xor` bit2
  carry = bit1 .&. bit2

fullAdder bit1 bit2 prevCarry = (sum, nextCarry)
 where
  (halfAdderSum, halfAdderCarry) = halfAdder bit1 bit2
  sum = halfAdderSum `xor` prevCarry
  nextCarry = (halfAdderSum .&. prevCarry) .|. (bit1 .&. bit2)

fullAdderPos nbr1 nbr2 prevCarry pos = 
 let
  bit1 = testBit nbr1 pos
  bit2 = testBit nbr2 pos
 in
  fullAdder bit1 bit2 prevCarry

adder nbr1 nbr2 = nbr1 + nbr2

printBits number
 | number == 0 = putChar('\n')
 | otherwise = do
  printBits(number `shiftR` 1)
  putStr . show $ (number .&. 1)


main = 
 do
  print (fullAdderPos 5 5 True 1)
 
