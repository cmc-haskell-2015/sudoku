module Main where

import Data.Functor
import Data.Char
import Data.String 

-------------------------------------------------------------------------------
main :: IO ()
-- main = playSudoku initField -- play on hard-coded field // for debug
main = do                      -- play on field from file
    greetingsMsg
    playFromFile 

-- =============== MAIN STRUCTURES ============================================
type Field = [[Int]]
data StepState = Possible | Impossible



-- =============== MANAGE GAME ================================================ 
-- request file name and read the field ---------------------------------------   
playFromFile :: IO ()
playFromFile = do
    print "Enter file name: "
    file <- getLine
    readField file

-- read the field and start the game on it ------------------------------------
readField :: FilePath -> IO ()
readField file = do
    strList <- lines <$> readFile file
    let field = (createField strList)    
    if (checkField field == True) then     
        playSudoku (createField strList)
    else
        print "Incorrect input puzzle"

-- transform input into field representation ----------------------------------
createField :: [String] -> Field
createField [] = []
createField (x:xs) = (parseString x) : (createField xs)

-- transform string into list of digits ---------------------------------------
parseString :: String -> [Int]
parseString [] = []
parseString (x:xs) | (isDigit x == True) = (ord x - ord '0') : parseString xs
                   | otherwise = parseString xs 

-- =============== AUXILIARY FUNCTIONS ========================================
greetingsMsg :: IO ()
greetingsMsg = print "Welcome to Sudoku puzzle game"

winMsg :: IO ()
winMsg = print "You win"

errMsg :: IO ()
errMsg = print "Something wrong :c" 

-- function for field hard-coding ---------------------------------------------
initField :: Field
initField = [[0,0,0,0,0,0,0,0,0], 
             [0,0,0,0,0,0,0,0,0],
             [0,0,0,0,0,0,0,0,0],
             [0,0,0,0,0,0,0,0,0],
             [0,0,0,0,0,0,0,0,0],
             [0,0,0,0,0,0,0,0,0],
             [0,0,0,0,0,0,0,0,0],
             [0,0,0,0,0,0,0,0,0],
             [0,0,0,0,0,0,0,0,0]]

-- print field to console -----------------------------------------------------             
printField :: Field -> IO ()
printField [] = return ()
printField (x:xs) = do 
    print x
    printField xs

-- check if the list contains the digit ---------------------------------------
isIn :: [Int] -> Int -> Bool
isIn [] _ = False
isIn (x:xs) y | y == x = True
              | otherwise = (isIn xs y)    
    
-- get the index for central cell of the square 3x3 area ----------------------
centerInd :: Int -> Int
centerInd i = 3 * (div i 3) + 1 
    
printStepResult :: StepState -> IO ()
printStepResult Possible = print "OK, the cell was modified"
printStepResult Impossible = print "Impossible step, try something else"
   
-- =============== GAME FUNCTIONS =============================================                   
-- put <num> digit in <col> position in the row ------------------------------- 
modifyRow :: [Int] -> Int -> Int -> [Int]
modifyRow r col num = (take col r) ++ (num : (drop (col+1) r))                 

-- put <num> digit in (<row>, <col>) cell on the field ------------------------                 
fillCell :: Field -> Int -> Int -> Int -> Field
fillCell f row col num = (take row f) ++ 
                         ((modifyRow (f !! row) col num) : (drop (row+1) f))                    

-- check line for correctness (does not contain similar digits) ---------------                         
checkLine :: [Int] -> Bool
checkLine l = checkLine_r l []  

checkLine_r :: [Int] -> [Int] -> Bool
checkLine_r [] _ = True
checkLine_r (x:xs) tmp | x == 0 = checkLine_r xs tmp
                       | isIn tmp x = False
                       | otherwise = checkLine_r xs (x:tmp)
                       

-- check row for correctness --------------------------------------------------              
checkRow :: Field -> Int -> Bool
checkRow f i = checkLine (f !! i)     

-- check column for correctness -----------------------------------------------
checkCol :: Field -> Int -> Bool
checkCol f i = checkLine (getCol f i)          

-- get column by its index ----------------------------------------------------
getCol :: Field -> Int -> [Int]
getCol [] _ = []
getCol (x:xs) i = (x !! i) : (getCol xs i)

-- check the square 3x3 area around the given cell for correctness ------------
checkSqr :: Field -> Int -> Int -> Bool
checkSqr f row col = checkLine (getSqr f (centerInd row) (centerInd col))

-- get square 3x3 area as a list of digits by the indices of it's single cell -
getSqr :: Field -> Int -> Int -> [Int]
getSqr f row col = [((f !! (row-1)) !! (col-1)) , 
                    ((f !! (row-1)) !! col)     , 
                    ((f !! (row-1)) !! (col+1)) ,
                    
                    ((f !! row) !! (col-1))     , 
                    ((f !! row) !! col)         , 
                    ((f !! row) !! (col+1))     ,
                       
                    ((f !! (row+1)) !! (col-1)) ,
                    ((f !! (row+1)) !! col)     , 
                    ((f !! (row+1)) !! (col+1))  ]

-- make one step of the game --------------------------------------------------                    
makeStep :: Field -> Int -> Int -> Int -> (Field, StepState)
makeStep f row col num | (checkStep (fillCell f row col num) row col == True) 
                              = ((fillCell f row col num), Possible)
                       | otherwise = (f, Impossible)

-- check that the step is possible in the moment ------------------------------
checkStep :: Field -> Int -> Int -> Bool
checkStep f row col = (checkRow f row) && 
                      (checkCol f col) &&
                      (checkSqr f row col)

-- check if there are any empty cells left ------------------------------------
checkFilled :: Field -> Bool
checkFilled [] = True
checkFilled (x:xs) = (not (isIn x 0)) && (checkWin xs)  

-- print results of the game --------------------------------------------------
checkWin :: Field -> Bool
checkWin f | (checkField == True) = winMsg
           | otherwise = errMsg

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
                 
-- main game function ---------------------------------------------------------
playSudoku :: Field -> IO ()
playSudoku field = do
    let gs = checkFilled field
    if (gs == True) then (checkWin f) else (playStep field) 

-- request input for the next step --------------------------------------------
playStep :: Field -> IO ()
playStep field = do
    print "---------------------------------------"
    print "Current field state: "
    print "---------------------------------------"
    printField field
    print "---------------------------------------"
    print "Make your next step"
    print "Enter row: "
    row <- getLine
    print "Enter col: "
    col <- getLine
    print "Enter digit: "
    num <- getLine
    let (f, st) = makeStep field (read row) (read col) (read num)
    printStepResult st
    playSudoku f    
    
    
-- ============================================================================
              

