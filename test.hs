fact :: Integer -> Integer
-- fact a
--  | a >= 1 = a * fact(a - 1)
--  | otherwise = 1

fact a = aux a 1
 where 
  aux a acc
   | a >= 1 = aux (a - 1) (a * acc)
   | otherwise = acc
-- fact a =
--  if (a >= 1)
--  then
--   a * fact(a - 1)
--  else
--   1