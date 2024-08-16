import Multiplier (multiplier)
import Adder (adder)
import Sub (sub)
main :: IO()

main = do
 print (adder 5 2)
 print (sub 5 2)
 print (multiplier 5 2)