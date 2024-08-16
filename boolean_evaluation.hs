module BooleanEval where
import Data.Word (Word32)
import Debug.Trace (trace)
import qualified Data.Map as Map
import Data.Bits (shiftL, xor, testBit, (.&.), (.|.))
import Data.List (null, tail)
import Data.Maybe (fromJust)

boolean_evaluation :: [Char] -> Bool
parser :: [Char] -> ([Bool], [Char])
evaluateExpresion :: [Bool] -> Char -> [Bool]
safeRemoveLastTwo :: [Bool] -> [Bool]
equivalenceFunc :: Bool -> Bool -> Bool
conditionFunc :: Bool -> Bool -> Bool
opMap :: [(Char, Bool -> Bool -> Bool)]
getOperator :: Char -> Bool -> Bool -> Bool

opMap = [
    ('&', (.&.)),
    ('|', (.|.)),
    ('^', xor),
    ('>', conditionFunc),
    ('=', equivalenceFunc)
  ]

-- safely remove last two elements from the list
safeRemoveLastTwo xs
    | length xs <= 2 = []
    | otherwise      = take (length xs - 2) xs

-- get the operator from the opMap
getOperator op = (fromJust $ lookup op opMap)

-- logic for `>`
conditionFunc op1 op2 = equivalenceFunc op1 op2 .|. op2

-- logic for `=`
equivalenceFunc op1 op2 = not (xor op1 op2)

-- `!` is a special operation that only takes one operand
evaluateExpresion vals '!' = newVals
 where
  newVals = init vals ++ [not (head vals)]

-- Implementation of the rest of operations
evaluateExpresion (op1:op2:vals) op = newVals
 where
  newVals = [(getOperator op) op1 op2] ++ vals

-- using list comprehension to filter the element into their respective lists
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

-- main = do
--  print(boolean_evaluation("10&")) -- False
--  print(boolean_evaluation("10|")) -- True
--  print(boolean_evaluation("11>")) -- True
--  print(boolean_evaluation("10=")) -- False
--  print(boolean_evaluation("1011||=")) -- True
--  print(boolean_evaluation("00&1|")) -- True
