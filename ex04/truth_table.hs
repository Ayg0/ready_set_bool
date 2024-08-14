import BooleanEval (boolean_evaluation) 
import Data.Word (Word32)
import Data.Bits (shiftL, xor, testBit, (.&.), (.|.))
import Data.List
import qualified Data.Map as Map

main:: IO()

print_truth_table :: [Char] -> IO()
printHead :: [Char] -> IO()
printVals :: [Char] -> [Char] -> IO()
printDashes :: [Char] -> IO()

getVariables :: [Char] -> ([Char], [Char])
iteratPossibleVars :: [Char] -> ([Char], [Char]) -> Int -> Int -> IO()
changeByEteration :: ([Char], [Char]) -> Int -> ([Char], [Char])
replaceWithVals :: [Char] -> ([Char], [Char]) -> [Char]
boolToChar :: Bool -> Char
getIndex :: [Char] -> Char -> Int
calcPower :: Int -> Int -> Int

----------------------------------------------------------------
calcPower base power = aux base power 1
   where
    aux base times res
     | times == 0 = res
     | otherwise = aux base (times - 1) (res * base)

----------------------------------------------------------------
getIndex list elem = aux list elem (length list) 0
 where
  aux list element listLength index
   | index == listLength = -1
   | otherwise = if elem == list !! index then index else aux list element listLength (index + 1)

----------------------------------------------------------------
replaceWithVals formula (varsList, values) =
 [if i /= -1 then values !! i else c | c <- formula, let i = getIndex varsList c]

-- getVariables (used nub to filter duplicates):
getVariables formula = (varsList, values)
 where
  varsList = nub [c | c <- formula, not (c `elem` "!&|^>=")]
  values =  ['0' | c <- varsList]

-- display variables:
printHead (var:varsList)
 | null varsList = putStrLn("| " ++ [var] ++" | = |")
 | otherwise = do
  putStr ("| " ++ [var] ++ " ")
  printHead varsList

-- display Dashes to separate the table:
printDashes (var:varsList)
 | null varsList = putStrLn("---------")
 | otherwise = do
  putStr ("----")
  printDashes varsList

----------------------------------------------------------------
boolToChar b = if b == True then '1' else '0'

-- print the values of each celll:
printVals (value:values) proposition
 | null values = putStrLn("| " ++ [value] ++" | " ++  [boolToChar (boolean_evaluation proposition)] ++" |")
 | otherwise = do
  putStr ("| " ++ [value] ++ " ")
  printVals values proposition

-- call with -1 to display the head of the table:
iteratPossibleVars formula (varsList, values) maxIters (-1) = do 
 printDashes varsList
 printHead varsList
 iteratPossibleVars formula (varsList, values) maxIters 0

-- Iterate through all possibilities:
iteratPossibleVars formula (varsList, values) maxIters iteration
 | iteration == maxIters = printDashes varsList
 | otherwise = do
  printVals values (replaceWithVals formula (varsList, values))
  iteratPossibleVars formula (changeByEteration (varsList, values) (iteration + 1)) maxIters (iteration + 1)

-- change values on iteration
changeByEteration (varsList, values) iteration = aux (varsList, values) iteration [] 0
 where
  aux (varsList, values) iteration newValues currentIndex
   | currentIndex == length varsList = (varsList, newValues)
   | otherwise = aux (varsList, values) iteration (newValues ++ [editedVal]) (currentIndex + 1)
    where
     editedVal = if ((iteration `mod` (calcPower 2 currentIndex)) == 0) then reversedVal else currentVal
      where
       currentVal = values !! currentIndex
       reversedVal = if currentVal == '1' then '0' else '1'

----------------------------------------------------------------
print_truth_table formula = iteratPossibleVars formula (varsList, values) maxIter (-1)
 where 
  (varsList, values) = (getVariables formula)
  maxIter = calcPower 2 (length varsList)

main = print_truth_table "AB^C&"