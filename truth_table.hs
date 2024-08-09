import Data.Word (Word32)
import Data.Bits (shiftL, xor, testBit, (.&.), (.|.))
import Data.List
import qualified Data.Map as Map

main:: IO()

print_truth_table :: [Char] -> IO()
printHead :: [Char] -> IO()
printVals :: [Char] -> IO()
printDashes :: [Char] -> IO()

getVariables :: [Char] -> ([Char], [Char])
factorial :: Int -> Int
iteratPossibleVars :: [Char] -> ([Char], [Char]) -> Int -> Int -> IO()
changeByEteration :: ([Char], [Char]) -> Int -> ([Char], [Char])

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

-- print the values of each celll:
printVals (value:values)
 | null values = putStrLn("| " ++ [value] ++" | K |")
 | otherwise = do
  putStr ("| " ++ [value] ++ " ")
  printVals values

-- Calculate the Factorial:
factorial nbr = aux nbr 1
 where
 aux nbr res
  | nbr <= 1 = res
  | otherwise = aux (nbr - 1) (res * nbr)

-- call with -1 to display the head of the table:
iteratPossibleVars formula (varsList, values) maxIters (-1) = do 
 printDashes varsList
 printHead varsList
 iteratPossibleVars formula (varsList, values) maxIters 0

-- Iterate through all possibilities:
iteratPossibleVars formula (varsList, values) maxIters iteration
 | iteration == maxIters = printDashes varsList
 | otherwise = do
  printVals values
  iteratPossibleVars formula (changeByEteration (varsList, values) (iteration + 1)) maxIters (iteration + 1)

-- change values on iteration
changeByEteration (varsList, values) iteration = aux (varsList, values) iteration [] 0
 where
  aux (varsList, values) iteration newValues currentIndex
   | currentIndex == length varsList = (varsList, newValues)
   | otherwise = aux (varsList, values) iteration (newValues ++ [editedVal]) (currentIndex + 1)
    where
     editedVal = if ((iteration `mod` (currentIndex + 1)) == 0) then reversedVal else currentVal
      where
       currentVal = values !! currentIndex
       reversedVal = if currentVal == '1' then '0' else '1'


print_truth_table formula = iteratPossibleVars formula (varsList, values) maxIter (-1)
 where 
  (varsList, values) = (getVariables formula)
  maxIter = factorial (length varsList)

main = print_truth_table "AB|C&"