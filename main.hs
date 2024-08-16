import Multiplier (multiplier)
import Adder (adder)
import GrayCode (gray_code)
import Sub (sub)
main :: IO()

main = do
-- adder
 putStrLn("ADDER: ")
 print (adder 5 2)
-- sub
 putStrLn("SUB: ")
 print (sub 5 2)
-- multiplier
 putStrLn("MULTIPLIER: ")
 print (multiplier 5 2)
-- grayCode
 putStrLn("GRAY_CODE: ")
 print(gray_code 0) 
 print(gray_code 1) 
 print(gray_code 2) 
 print(gray_code 3) 
 print(gray_code 4) 
 print(gray_code 5) 
 print(gray_code 6) 
 print(gray_code 7) 
 print(gray_code 8) 
