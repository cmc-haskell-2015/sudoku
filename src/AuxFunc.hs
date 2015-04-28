module AuxFunc where

-- to be used in 'do'-blocks --------------------------------------------------
return_same :: a -> a
return_same x = x

-- check if the list contains the digit ---------------------------------------
isIn :: [Int] -> Int -> Bool
isIn [] _ = False
isIn (x:xs) y | y == x = True
              | otherwise = (isIn xs y)    

-- get the index for central cell of the square 3x3 area ----------------------
centerInd :: Int -> Int
centerInd i = 3 * (div i 3) + 1  

-- =============== MASSAGES ===================================================
greetingsMsg :: IO ()
greetingsMsg = print "Welcome to Sudoku puzzle game" 