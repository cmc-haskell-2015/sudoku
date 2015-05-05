module Game where
import Types
        
-- =============== AUXILIARY FUNCTIONS ========================================        
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

greetingsMsg :: IO ()
greetingsMsg = print "Welcome to Sudoku puzzle game" 

-- =============== GAME FUNCTIONS =============================================
-- main game function ---------------------------------------------------------
playGame :: World -> IO ()
playGame (f, ms, gs, t) = do
    if (gs == InProgress) then do 
        playMove (f, ms, gs, t)
    else do
        print gs
      
-- request input for the next move --------------------------------------------
playMove :: World -> IO ()
playMove (f0, ms0, gs0, t) = do
    printField f0
    print "Make your next step"
    print "Enter row: "
    row <- getLine
    print "Enter col: "
    col <- getLine
    print "Enter digit: "
    num <- getLine
    let (f1, ms1, gs1, t)=makeMove(f0,ms0,gs0,t)(read row)(read col)(read num)
    print ms1
    playGame (f1, ms1, gs1, t)                  

-- make move: try to fill [num] in ([row],[col]) cell -------------------------          
makeMove :: World -> Int -> Int -> Int -> World               
makeMove (f0, ms0, gs0, t) row col num = do
    let c = getCell f0 row col
--    if (c == (Fixed 0)) then do
--        return_same (f0, ErrFixed, gs0, t)
--    else do        
    let f1 = fillCell f0 row col num
--        let chk = checkFill f1 row col 
--        if (chk == True) then do
    let gs1 = getGameState f1
    return_same (f1, Selected(row,col), gs1, t)
--        else do 
--            return_same (f0, ErrImpossible, gs0, t)    

-- returns current game state -------------------------------------------------
getGameState :: Field -> GameState
getGameState f = do
    let chkFin = checkFinished f
    if (chkFin == False) then do
        return_same InProgress
    else do
        let chkFld = checkField f
        if (chkFld == True) then do
            return_same Finished
        else do
            return_same Error
            
-- put <num> digit in <col> position in the row ------------------------------- 
modifyRow :: [Cell] -> Int -> Int -> [Cell]
modifyRow r col num = (take col r) ++ ((Filled num) : (drop (col+1) r))                 

-- clear (<row>, <col>) cell --------------------------------------------------
clearCell :: World -> Int -> Int -> World
clearCell (f0,ms0,gs0,t) row col = do
    let c = getCell f0 row col
    if (c == (Fixed 0)) then do
        return_same (f0, ErrFixed, gs0,t)
    else do      
        return_same (((take row f0) ++ 
                        (((take col (f0 !! row)) ++ 
                        ((Empty) : drop (col+1) (f0 !! row))) : 
                     (drop (row+1) f0))), 
                     Selected(row,col), 
                     InProgress,t)
                            
-- put <num> digit in (<row>, <col>) cell on the field ------------------------                 
fillCell :: Field -> Int -> Int -> Int -> Field
fillCell f row col num = (take row f) ++ 
                         ((modifyRow (f !! row) col num) : (drop (row+1) f))   

-- return cell by its indices -------------------------------------------------                         
getCell :: Field -> Int -> Int -> Cell
getCell f row col = (f !! row) !! col 
    
-- check line for correctness (does not contain similar digits) ---------------                         
checkLine :: [Cell] -> Bool
checkLine l = checkLine_r l []  

checkLine_r :: [Cell] -> [Int] -> Bool
checkLine_r [] _ = True
checkLine_r (Empty:xs) tmp = checkLine_r xs tmp
checkLine_r ((Fixed x):xs) tmp  | isIn tmp x = False
                                | otherwise = checkLine_r xs (x:tmp)
checkLine_r ((Filled x):xs) tmp | isIn tmp x = False
                                | otherwise = checkLine_r xs (x:tmp)                            
             
-- check row for correctness --------------------------------------------------              
checkRow :: Field -> Int -> Bool
checkRow f i = checkLine (f !! i)     

-- check column for correctness -----------------------------------------------
checkCol :: Field -> Int -> Bool
checkCol f i = checkLine (getCol f i)          

-- get column by its index ----------------------------------------------------
getCol :: Field -> Int -> [Cell]
getCol [] _ = []
getCol (x:xs) i = (x !! i) : (getCol xs i)

-- check the square 3x3 area around the given cell for correctness ------------
checkSqr :: Field -> Int -> Int -> Bool
checkSqr f row col = checkLine (getSqr f (centerInd row) (centerInd col))

-- get square 3x3 area as a list of digits by the indices of it's single cell -
getSqr :: Field -> Int -> Int -> [Cell]
getSqr f row col = [((f !! (row-1)) !! (col-1)) , 
                    ((f !! (row-1)) !! col)     , 
                    ((f !! (row-1)) !! (col+1)) ,
                    
                    ((f !! row) !! (col-1))     , 
                    ((f !! row) !! col)         , 
                    ((f !! row) !! (col+1))     ,
                       
                    ((f !! (row+1)) !! (col-1)) ,
                    ((f !! (row+1)) !! col)     , 
                    ((f !! (row+1)) !! (col+1))  ]

-- check that the move is possible in the moment ------------------------------
checkFill :: Field -> Int -> Int -> Bool
checkFill f row col = (checkRow f row) && 
                      (checkCol f col) &&
                      (checkSqr f row col)  
    
-- check all the field for correctness ----------------------------------------
checkField :: Field -> Bool 
checkField f = (checkAllRows f) && (checkAllCols f) && (checkAllSqrs f)

checkAllRows :: Field -> Bool
checkAllRows [] = True
checkAllRows (x:xs) = (checkLine x) && (checkAllRows xs)

checkAllCols :: Field -> Bool
checkAllCols f = checkAllCols_r f 0
checkAllCols_r :: Field -> Int -> Bool
checkAllCols_r f i | (i == 8) = (checkCol f i)
                   | otherwise = (checkCol f i) && (checkAllCols_r f (i+1))

checkAllSqrs :: Field -> Bool
checkAllSqrs f = (checkSqr f 1 1) && (checkSqr f 1 4) && (checkSqr f 1 7) &&
                 (checkSqr f 4 1) && (checkSqr f 4 4) && (checkSqr f 4 7) &&
                 (checkSqr f 7 1) && (checkSqr f 7 4) && (checkSqr f 7 7)
                     
-- check if there are any empty cells left ------------------------------------
checkFinished :: Field -> Bool
checkFinished [] = True
checkFinished (x:xs) = (checkRowFinished x) && (checkFinished xs)      

checkRowFinished :: [Cell] -> Bool
checkRowFinished [] = True
checkRowFinished (Empty:xs) = False
checkRowFinished (x:xs) = checkRowFinished xs  

-- check move (without doing it) ----------------------------------------------
checkMove :: Field -> Int -> Int -> Int -> Bool
checkMove f row col num = checkFill (fillCell f row col num) row col

-- possible solutions for a cell ----------------------------------------------
getHint :: Field -> Int -> Int -> [Int]
getHint f row col = getHint_r f row col [1,2..9]

getHint_r :: Field -> Int -> Int -> [Int] -> [Int]
getHint_r f row col [] = []
getHint_r f row col (x:xs) | ((checkMove f row col x) == True) = 
                               x : (getHint_r f row col xs)
                           | otherwise = (getHint_r f row col xs)                              
 
 