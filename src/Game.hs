module Game where

import Types
        
-- =============== AUXILIARY FUNCTIONS ========================================        

-- check if the list contains the digit ---------------------------------------
isIn :: [Int] -> Int -> Bool
isIn [] _ = False
isIn (x:xs) y | y == x = True
              | otherwise = (isIn xs y)    

-- get the index for central cell of the square 3x3 area ----------------------
centerInd :: Int -> Int
centerInd i = 3 * (div i 3) + 1  

-- =============== GAME FUNCTIONS =============================================

-- make move: try to fill [num] in ([row],[col]) cell -------------------------          
makeMove :: World -> Int -> Int -> Int -> World               
makeMove w row col num = w'
    where
        f'  = fillCell (field w) row col num
        w'  = w { field = f'
                , moveState = Selected (row, col)
                , gameState = getGameState f'
                }

-- returns current game state -------------------------------------------------
getGameState :: Field -> GameState
getGameState f =
    if not isFin 
        then InProgress
        else if isCorrFld 
            then Finished
            else Error
    where 
        isFin = checkFinished f
        isCorrFld = checkField f  
        
-- clear (<row>, <col>) cell --------------------------------------------------
clearCell :: World -> Int -> Int -> World
clearCell w row col =
    if isFixed c 
        then w { moveState = ErrFixed }
    else w { field = setCell Empty row col f
           , moveState = Selected (row, col)
           , gameState = InProgress }
    where
        f = field w
        c = getCell f row col

-- put <num> digit in (<row>, <col>) cell on the field ------------------------                 
fillCell :: Field -> Int -> Int -> Int -> Field
fillCell f row col num = setCell (Filled num) row col f    
    
-- set the cell in the (<row>, <col>) position on the field -------------------    
setCell :: Cell -> Int -> Int -> Field -> Field
setCell c row col f = left ++ (top ++ c : bottom) : right
    where
        (left, mid : right) = splitAt row f
        (top, _ : bottom) = splitAt col mid

-- return cell by its indices -------------------------------------------------                         
getCell :: Field -> Int -> Int -> Cell
getCell f row col = (f !! row) !! col 
    
-- check line for correctness -------------------------------------------------
-- means that the line of cells does not contain similar cells ----------------                        
checkLine :: [Cell] -> Bool
checkLine l = checkLine_r l []  

checkLine_r :: [Cell] -> [Int] -> Bool
checkLine_r [] _ = True
checkLine_r (Empty : xs) tmp = checkLine_r xs tmp
checkLine_r ((Fixed x) : xs) tmp  | isIn tmp x = False
                                  | otherwise = checkLine_r xs (x : tmp)
checkLine_r ((Filled x) : xs) tmp | isIn tmp x = False
                                  | otherwise = checkLine_r xs (x : tmp)               

-- check <i> row for correctness ----------------------------------------------
-- row is represented as a line -----------------------------------------------              
checkRow :: Field -> Int -> Bool
checkRow f i = checkLine (f !! i)     

-- check <j> column for correctness -------------------------------------------
-- column is represented as a line --------------------------------------------

checkCol :: Field -> Int -> Bool
checkCol f j = checkLine (getCol f j)          

-- get column by its index ----------------------------------------------------
getCol :: Field -> Int -> [Cell]
getCol [] _ = []
getCol (x:xs) i = (x !! i) : (getCol xs i)

-- check the square 3x3 area around the given cell for correctness ------------
-- the area is represented as a line ------------------------------------------
checkSqr :: Field -> Int -> Int -> Bool
checkSqr f row col = checkLine (getSqr f (centerInd row) (centerInd col))

-- get square 3x3 area as a list of digits by the indices of it's single cell -
getSqr :: Field -> Int -> Int -> [Cell]
getSqr f row col = 
    [f !! i !! j 
        | i <- [row - 1, row, row + 1],
          j <- [col - 1, col, col + 1]]               

-- check that the move is possible at the moment ------------------------------
checkFill :: Field -> Int -> Int -> Bool
checkFill f row col = checkRow f row && 
                      checkCol f col &&
                      checkSqr f row col  
    
-- check all the field for correctness ----------------------------------------
checkField :: Field -> Bool 
checkField f = checkAllRows f && checkAllCols f && checkAllSqrs f

checkAllRows :: Field -> Bool
checkAllRows [] = True
checkAllRows (x : xs) = (checkLine x) && (checkAllRows xs)

checkAllCols :: Field -> Bool
checkAllCols f = checkAllCols_r f 0

checkAllCols_r :: Field -> Int -> Bool
checkAllCols_r f i | i == 8 = checkCol f i
                   | otherwise = checkCol f i && checkAllCols_r f (i + 1)

checkAllSqrs :: Field -> Bool
checkAllSqrs f = checkSqr f 1 1 && checkSqr f 1 4 && checkSqr f 1 7 &&
                 checkSqr f 4 1 && checkSqr f 4 4 && checkSqr f 4 7 &&
                 checkSqr f 7 1 && checkSqr f 7 4 && checkSqr f 7 7
                     
-- check if there are any empty cells left ------------------------------------
checkFinished :: Field -> Bool
checkFinished [] = True
checkFinished (x : xs) = (checkRowFinished x) && (checkFinished xs)      

checkRowFinished :: [Cell] -> Bool
checkRowFinished = all (/= Empty)

-- check move for possibility (without doing it) ------------------------------
isPossible :: Field -> Int -> Int -> Int -> Bool
isPossible f row col num = checkFill (fillCell f row col num) row col

-- possible solutions for a cell ----------------------------------------------
getHint :: Field -> Int -> Int -> [Int]
getHint f row col = getHint_r f row col [1,2..9]

getHint_r :: Field -> Int -> Int -> [Int] -> [Int]
getHint_r _ _ _ [] = []
getHint_r f row col (x : xs) 
    | isPossible f row col x   = x : (getHint_r f row col xs)
    | otherwise                = (getHint_r f row col xs)                     
