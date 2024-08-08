import Data.Word (Word32)
import qualified Data.Map as Map
import Data.Bits (shiftL, xor, testBit, (.&.), (.|.))
import Data.List (null, tail)


main:: IO()
boolean_evaluation :: [Char] -> Bool
parser :: [Char] -> ([Bool], [Char])
evaluateExpresion :: [Bool] -> Char -> [Bool]
safeRemoveLastTwo :: [Bool] -> [Bool]
equivalenceFunc :: Bool -> Bool -> Bool
conditionFunc :: Bool -> Bool -> Bool

conditionFunc op1 op2 = equivalenceFunc op1 op2 .|. op2
equivalenceFunc op1 op2 = not (xor op1 op2)

safeRemoveLastTwo xs
    | length xs <= 2 = []
    | otherwise      = take (length xs - 2) xs

evaluateExpresion vals '!' = newVals
 where
  newVals = init vals ++ [not (last vals)]

evaluateExpresion vals '&' = newVals
 where
  op1 = last vals
  op2 = last (init vals)
  newVals = safeRemoveLastTwo vals ++ [(op1 .&. op2)]

evaluateExpresion vals '|' = newVals
 where
  op1 = last vals
  op2 = last (init vals)
  newVals = safeRemoveLastTwo vals ++ [(op1 .|. op2)]

evaluateExpresion vals '^' = newVals
 where
  op1 = last vals
  op2 = last (init vals)
  newVals = safeRemoveLastTwo vals ++ [(op1 `xor` op2)]

evaluateExpresion vals '>' = newVals
 where
  op1 = last vals
  op2 = last (init vals)
  newVals = safeRemoveLastTwo vals ++ [(op1 `conditionFunc` op2)]

evaluateExpresion vals '=' = newVals
 where
  op1 = last vals
  op2 = last (init vals)
  newVals = safeRemoveLastTwo vals ++ [(op1 `equivalenceFunc` op2)]


parser proposition = (vals, ops)
 where
  vals = [if c == '1' then True else False | c <- proposition, c == '1' || c == '0']
  ops = [c | c <- proposition, c `elem` "!&|^>="]

boolean_evaluation proposition = aux (parser proposition)
 where
  aux (vals, ops)
   | length ops == 0 = head vals
   | otherwise = aux (newVals, tail ops)
    where
     newVals = evaluateExpresion vals (head ops)

main = do
 print(boolean_evaluation("10&"))
 print(boolean_evaluation("10|"))
 print(boolean_evaluation("11>"))
 print(boolean_evaluation("10="))
 print(boolean_evaluation("1011||="))
