import Data.Word (Word32)
import Data.Bits (shiftL, xor, testBit, (.&.), (.|.))
import Data.List
import qualified Data.Map as Map

main:: IO()
print_truth_table :: [Char] -> IO()
printTableHead :: ([Char], [Char]) -> IO()
getVariables :: [Char] -> ([Char], [Char])
factorial :: Int -> Int
iteratPossibleVars :: [Char] -> ([Char], [Char]) -> Int -> Int -> IO()
changeByEteration :: ([Char], [Char]) -> Int -> ([Char], [Char])

-- getVariables (used nub to filter duplicates)
getVariables formula = (varsList, values)
 where
  varsList = nub [c | c <- formula, not (c `elem` "!&|^>=")]
  values =  ['0' | c <- varsList]

-- print the head of the table
printTableHead (varsList, values)
 | null varsList = print "Why Are you doing this ??"
 | otherwise = aux varsList
  where
   aux (var:varsList)
    | null varsList = putStrLn(" | " ++ [var] ++" | = |")
    | otherwise = do
     putStr ("| " ++ [var])
     aux varsList

-- Calculate the Factorial:
factorial nbr = aux nbr 1
 where
 aux nbr res
  | nbr <= 1 = res
  | otherwise = aux (nbr - 1) (res * nbr)

-- -- Iterate through all possibilities:
iteratPossibleVars formula (varsList, values) maxIters iteration
 | iteration == maxIters = print "WoW"
 | otherwise = do
  print(varsList, values)
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

print_truth_table formula = iteratPossibleVars formula (varsList, values) maxIter 0
 where 
  (varsList, values) = (getVariables formula)
  maxIter = factorial (length varsList)

main = print_truth_table "ABC&C"