module Interface where

import Types
import Game
import Records

import Data.Char
import Data.Monoid 

import Graphics.Gloss.Interface.IO.Game

-- =============== AUXILIARY FUNCTIONS ========================================
winHeight :: Float -- game window height
winHeight = 540

winWidth :: Float -- game window width
winWidth = (winHeight + 3 * cellSize) + 100 -- the second item stands for the
                                            -- interval between field 
                                            -- and numberpad

tlppX :: Float -- top left pixel position (X)                               
tlppX = - winWidth / 2 

tlppY :: Float -- top left pixel position (Y)
tlppY = winHeight / 2  

trppX :: Float -- top right pixel position (X)
trppX = winWidth / 2   

trppY :: Float -- top right pixel position (Y)
trppY = winHeight / 2  

blppX :: Float -- bottom left pixel position (X)
blppX = tlppX      

blppY :: Float -- bottom left pixel position (Y)      
blppY = - tlppY          

brppX :: Float -- bottom right pixel position (X)
brppX = trppX    

brppY :: Float -- bottom right pixel position (Y)        
brppY = - trppY          

cellSize :: Float -- size of a cell 
cellSize = winHeight / 9  

tlcpX :: Float -- top left cell position (X)
tlcpX = (cellSize / 2) - (winWidth / 2)
 
tlcpY :: Float -- top left cell position (Y)
tlcpY = - (cellSize / 2) + (winHeight / 2)

trnpX :: Float -- top right number position (X)
trnpX = (winWidth / 2) - (cellSize / 2)
 
trnpY :: Float -- top right number position (Y)
trnpY = (winHeight / 2) - (cellSize / 2) 

digScl :: Float -- digits scale to fit the cells 
digScl = (cellSize / 50) * 0.3 

cellPosX :: Int -> Float -- cell X coordinate by its column index
cellPosX col = tlcpX + cellSize * (fromIntegral col)

cellPosY :: Int -> Float -- cell Y coordinate by its row index
cellPosY row = tlcpY - cellSize * (fromIntegral row)

numPosX :: Float -> Float -- number X coordinate according to cell X coordinate
numPosX posX = posX - (cellSize / 4)

numPosY :: Float -> Float -- number Y coordinate according to cell Y coordinate
numPosY posY = posY - (cellSize / 4)

rowByY :: Float -> Int -- cell row index by Y coordinate
rowByY y = 8 - floor ( (y + winHeight / 2) / cellSize )

colByX :: Float -> Int -- cell column index by X coordinate
colByX x 
    | x <= tlppX + 9 * cellSize = floor ( (x + winWidth / 2) / cellSize )
    | otherwise                 = (-1)
         
numByXY :: Float -> Float -> Int -- chosen number on the numberpad     
numByXY x y = ncol + (3 * nrow) + 1
    where
        nrow = 2 - floor ( (y - winHeight / 2 + 3 * cellSize) / cellSize ) 
        ncol = floor ( (x - winWidth / 2 + 3 * cellSize) / cellSize )
    
-- =============== DRAWING FUCTIONS ===========================================
-- world ----------------------------------------------------------------------         
drawWorld :: World -> IO Picture
drawWorld (World f ms gs t) =
    case gs of
        Finished -> return (drawFinish <> drawFinishTime t) 
        ShowInfo -> return (drawInfo <> drawTime t)
        Result   -> return (drawFinish <> drawFinishTime t)
        Error    -> return (drawField f ms 
                    <> drawNumberpad 
                    <> drawInfoSuggest
                    <> drawTime t
                    <> drawError)
        _ -> return (drawField f ms 
             <> drawNumberpad 
             <> drawInfoSuggest
             <> drawTime t)

-- time -----------------------------------------------------------------------
drawTime :: Float -> Picture
drawTime t = translate (tlppX + 9 * cellSize + 5) 
                       (brppY + 5)
                           (color blue
                               (scale (0.5 * digScl) (0.5 * digScl)
                                   (text "time:")))
          <> translate (tlppX + 10 * cellSize + 10) 
                       (brppY + 5)
                           (color blue
                               (scale (0.5 * digScl) (0.5 * digScl)
                                   (text((show mins) ++ " min " ++
                                         (show secs) ++ " sec"))))                                   
    where 
        s = floor t
        mins = div s 60
        secs = mod s 60

drawFinishTime :: Float -> Picture
drawFinishTime t = do
  translate (-4 * cellSize) 0
                       (color blue (scale digScl digScl
                           (text "time:")))
                <> translate (- cellSize) 0
                           (color blue (scale digScl digScl
                                (text((show mins) ++ " min " ++
                                      (show secs) ++ " sec"))))                                    
    where 
        s = floor t
        mins = div s 60
        secs = mod s 60         
                        
-- info -----------------------------------------------------------------------
drawInfoSuggest :: Picture
drawInfoSuggest = translate (tlppX + 9 * cellSize + 5) 
                            (brppY + 3 * cellSize / 2) 
                       (scale (0.4 * digScl) (0.4 * digScl) 
                           (text "Press F1 to show Info"))

drawInfo :: Picture
drawInfo = drawHeadline
        <> drawHowToPlay 
        <> drawCloseInfo     

drawSolve :: Field -> Picture
drawSolve f = drawAllCells f 
             <> drawBoldLines
             <> drawGrid

drawHeadline :: Picture
drawHeadline = translate tlppX (tlppY - cellSize / 2)
                   (scale digScl digScl
                       (text "========================"))                   
            <> translate (tlppX + 5 * cellSize) (0.9 * (tlppY - cellSize))
                   (scale (1.2 * digScl) (1.2 * digScl)
                       (text "SUDOKU"))  
            <> translate tlppX (tlppY - 2 * cellSize)
                   (scale digScl digScl
                       (text "========================"))  

drawHowToPlay :: Picture
drawHowToPlay = translate (tlppX + cellSize / 2) (tlppY - 3 * cellSize) 
                       (scale (0.5 * digScl) (0.5 * digScl) 
                           (text ("Select a cell that you want to " ++
                                  "(re)fill by mouse click.")))
             <> translate (tlppX + cellSize / 2) (tlppY - 7 * cellSize / 2) 
                       (scale (0.5 * digScl) (0.5 * digScl) 
                             (text ("Then choose a digit " ++ 
                                    "from the numberpad.")))
             <> translate (tlppX + cellSize / 2) (tlppY - 9 * cellSize / 2) 
                       (scale (0.4 * digScl) (0.4 * digScl) 
                             (text ("Right mouse click (or press 'h') " ++ 
                                    "on a cell will show possible numbers.")))                                    
             <> translate (tlppX + cellSize / 2) (tlppY - 11 * cellSize / 2) 
                       (scale (0.35 * digScl) (0.35 * digScl) 
                             (text ("You can use either keyboard " ++ 
                                    "(arrows + numbers + space) or" ++
                                    " the given numpad.")))
                        
drawCloseInfo :: Picture
drawCloseInfo = translate (blppX + cellSize / 2) (blppY + 5)
                    (scale (0.5 * digScl) (0.5 * digScl)
                        (text "Press F1 to close Info"))                    
                                    
-- game over ------------------------------------------------------------------
drawFinish :: Picture
drawFinish = translate (-5 * cellSize / 2) (2 * cellSize) 
                 (scale (1.5 * digScl) (1.5 * digScl) 
                     (text "YOU WIN"))

-- selected cell overlay ------------------------------------------------------                                      
drawSelectedCell :: Int -> Int -> Picture
drawSelectedCell srow scol = 
    color green (drawCell Empty (cellPosX scol) (cellPosY srow)
                          cellSize cellSize)  

-- error ----------------------------------------------------------------------
drawError :: Picture
drawError = translate posX 0
                (color clr (scale txtScl txtScl
                    (text "Smth wrong")))                          
         <> translate posX (- cellSize / 2)
                (color clr (scale txtScl txtScl
                    (text "check for errors")))
    where 
        posX = tlppX + 9 * cellSize + 5        
        txtScl = 0.45 * digScl 
        clr = dark red           
                                           
-- hint -----------------------------------------------------------------------
drawHint :: Field -> Int -> Int -> Picture
drawHint f hrow hcol =
    if (hint /= []) 
        then drawSelectedCell hrow hcol
             <> translate posX 0
                    (color clr (scale txtScl txtScl
                        (text "Possible for selected cell:")))                          
             <> translate posX (- cellSize / 2)
                    (color clr (scale txtScl txtScl
                        (text (show hint))))
        else drawSelectedCell hrow hcol <> drawError    
    where
        hint = getHint f hrow hcol
        posX = tlppX + 9 * cellSize + 5        
        txtScl = 0.45 * digScl 
        clr = dark green 
        
-- field ----------------------------------------------------------------------                           
drawField :: Field -> MoveState -> Picture
drawField f (Selected (srow, scol)) = drawAllCells f 
                                   <> drawBoldLines
                                   <> drawGrid
                                   <> drawSelectedCell srow scol
                                   
drawField f (Hint (hrow, hcol)) = drawAllCells f
                               <> drawBoldLines                                   
                               <> drawGrid
                               <> drawHint f hrow hcol
                               
drawField f _ = drawAllCells f 
             <> drawBoldLines
             <> drawGrid
             
-- grid ----------------------------------------------------------------------- 
drawGrid :: Picture
drawGrid = pictures [(drawCell Empty (cellPosX col) (cellPosY row) 
                               cellSize cellSize) 
                        | row <- [0,1 ..8],  
                          col <- [0,1 ..8]] 
                                
-- additional lines between 3x3 squares for comfort ---------------------------                      
drawBoldLines :: Picture
drawBoldLines = pictures
    [drawCell Empty (cellPosX col) (cellPosY row) 
                    (3 * cellSize - 2) (3 * cellSize - 2)            
    | col <- [1,4,7],
      row <- [1,4,7]]    
              
-- single cell ----------------------------------------------------------------
drawCell :: Cell -> Float -> Float -> Float -> Float -> Picture
drawCell Empty posX posY sizeX sizeY = 
    translate posX posY (rectangleWire sizeX sizeY)
        
drawCell (Filled n) posX posY sizeX sizeY = 
    pictures [translate posX posY (rectangleWire sizeX sizeY),
              translate (numPosX posX) (numPosY posY) 
                  (scale digScl digScl (text (show n)))]   
                  
drawCell (Fixed n) posX posY sizeX sizeY = 
    pictures [color clr (translate posX posY (rectangleSolid sizeX sizeY)),
              translate (numPosX posX) (numPosY posY) 
                  (scale digScl digScl (text (show n)))] 
    where 
        clr = dark white    
                  
-- all the cells in the field -------------------------------------------------
drawAllCells :: Field -> Picture
drawAllCells f = pictures [(drawCell (getCell f row col)
                                     (cellPosX col) (cellPosY row) 
                                     cellSize cellSize) 
                               | row <- [0,1 ..8],  
                                 col <- [0,1 ..8]] 
                                 
-- numberpad ------------------------------------------------------------------                  
drawNumberpad :: Picture
drawNumberpad = drawClearButton <> pictures
    [ drawCell (Filled n) offsetX offsetY cellSize cellSize
    | (n, (i, j)) <- numbers
    , let offsetX = trnpX - i * cellSize
    , let offsetY = trnpY - j * cellSize ]
    where
        numbers = zip [1..9] coords
        coords  = [ (i, j) | j <- [0..2], i <- [2, 1, 0] ]

drawClearButton :: Picture
drawClearButton = drawCell Empty 
                          (trnpX - 1 * cellSize) (trnpY - 3 * cellSize)
                          (3 * cellSize) cellSize
               <> translate 
                      (trnpX - 2 * cellSize)
                      (trnpY - 13 * cellSize / 4)                
                          (scale (0.8 * digScl) (0.8 * digScl) 
                              (text "CLEAR"))                
                  
-- =============== EVENTS HANDLER =============================================

-- main handler function ------------------------------------------------------
-- defines what action to do according to mouse click coordinates -------------    
handleWorld :: Event -> World -> IO World
handleWorld _ w@World{ gameState = Finished } = return w

handleWorld (EventKey (MouseButton LeftButton) Down _ (x, y)) w 
    = return (handleMouseLeft w x y)
handleWorld (EventKey (MouseButton RightButton) Down _ (x, y)) w  
    = return (handleMouseRight w x y)
  
handleWorld (EventKey (SpecialKey KeyF1) Down _ _) w 
    = case gameState w of
          Result -> return w
          _        -> return (handleInfo w)

handleWorld (EventKey (SpecialKey KeySpace) Down _ _) 
            w@World{ moveState = Selected (row, col) } 
    = return (clearCell w row col)
handleWorld (EventKey (SpecialKey KeySpace) Down _ _) 
            w@World{ moveState = Hint (row, col) } 
    = return (clearCell w row col)
  
handleWorld (EventKey (Char c) Down _ _) w =
    case moveState w of
        Selected _ -> return (handleChar c w)
        Hint     _ -> return (handleChar c w)
        _ -> return w

handleWorld (EventKey (SpecialKey KeyRight) Down _ _) w  
    = return (handleRight w)  
handleWorld (EventKey (SpecialKey KeyLeft) Down _ _) w  
    = return (handleLeft w) 
handleWorld (EventKey (SpecialKey KeyUp) Down _ _) w 
    = return (handleUp w) 
handleWorld (EventKey (SpecialKey KeyDown) Down _ _) w
    = return (handleDown w) 
              
handleWorld _ w = return w    

-- handlers for mouse buttons -------------------------------------------------
-------------------------------------------------------------------------------
handleMouseLeft :: World -> Float -> Float -> World
handleMouseLeft w x y =     
    if fieldHit x y    
        then handleSelect w x y
        else if numpadHit x y 
                 then handleMove w x y
                 else w

-------------------------------------------------------------------------------
handleMouseRight :: World -> Float -> Float -> World
handleMouseRight w x y = 
    if fieldHit x y    
        then handleHint w x y
        else w                  

-------------------------------------------------------------------------------        
-- handler for common buttons -------------------------------------------------                 
handleChar :: Char -> World -> World
handleChar '0' w = w
handleChar c w =
    case getSelected (moveState w) of
        Nothing -> w
        Just (row, col) ->
            if isDigit c
                then makeMove w { moveState = Selected (row, col) } 
                              row col (ord c - ord '0')
            else if c == 'h' 
                     then w { moveState = Hint (row,col) }
                     else w

-------------------------------------------------------------------------------
-- handler for arrows ---------------------------------------------------------
handleDir :: (Field -> Int -> Int -> (Int, Int)) -> World -> World
handleDir closest w =
    case getSelected (moveState w) of
        Nothing -> w { moveState = Selected (closestNotFixed (field w) 0 0) }
        Just (row, col) ->
            let (row', col') = closest (field w) row col
            in if check row' col'
                   then w { moveState = Selected (row', col') }
                   else w { moveState = Selected (row, col) }
    where
        check row col = row >= 0 && row < 9 && col >= 0 && col < 9

closestNotFixed :: Field -> Int -> Int -> (Int, Int)
closestNotFixed f row col 
    | (col == 8) && (isFixed(getCell f row col))= closestNotFixed f (row+1) 0
    | (isFixed (getCell f row col))             = closestNotFixed f row (col+1)   
    | otherwise                                 = (row, col)
    
-------------------------------------------------------------------------------
handleRight :: World -> World
handleRight = handleDir closestRight

closestRight :: Field -> Int -> Int -> (Int, Int)
closestRight f row col | ((col == 8) && (isFixed (getCell f row col)))
                             = (row,9)
                       | ((col <= 7) && (isFixed (getCell f row (col+1))))
                             = closestRight f row (col+1) 
                       | otherwise = (row, col+1)
-------------------------------------------------------------------------------
handleLeft :: World -> World
handleLeft = handleDir closestLeft

closestLeft :: Field -> Int -> Int -> (Int, Int)
closestLeft f row col | ((col == 0) && (isFixed (getCell f row col)))
                             = (row,-1)
                       | ((col >= 1) && (isFixed (getCell f row (col-1))))
                             = closestLeft f row (col-1) 
                       | otherwise = (row, col-1)          
                       
-------------------------------------------------------------------------------
handleUp :: World -> World
handleUp = handleDir closestUp

closestUp :: Field -> Int -> Int -> (Int, Int)
closestUp f row col    | ((row == 0) && (isFixed (getCell f row col)))
                             = (-1,col)
                       | ((row >= 1) && (isFixed (getCell f (row-1) (col))))
                             = closestUp f (row-1) (col) 
                       | otherwise = (row-1, col)    
                       
-------------------------------------------------------------------------------
handleDown :: World -> World
handleDown = handleDir closestDown

closestDown :: Field -> Int -> Int -> (Int, Int)
closestDown f row col  | row == 8 && isFixed (getCell f row col)
                             = (9,col)
                       | row <= 7 && isFixed (getCell f (row+1) col)
                             = closestDown f (row+1) col
                       | otherwise = (row+1, col) 

-------------------------------------------------------------------------------                       
-- select cell in the field ---------------------------------------------------
handleSelect :: World -> Float -> Float -> World
handleSelect w x y = do 
    if isFixed c
      then w
      else w { moveState = Selected (row, col) }
    where
        row = rowByY y 
        col = colByX x
        c = getCell (field w) row col 
        
-- make move ------------------------------------------------------------------    
handleMove :: World -> Float -> Float -> World
handleMove w x y =
    case moveState w of
        Selected (row, col) -> handleSelectedCell row col    
        Hint     (row, col) -> handleSelectedCell row col
        _ -> w
    where
        handleSelectedCell row col = do
            let num = numByXY x y
            if (num <= 9) 
                then makeMove w { moveState = Selected (row, col) } row col num   
                else clearCell w { moveState = Selected (row, col) } row col

-- show hint ------------------------------------------------------------------
handleHint :: World -> Float -> Float -> World
handleHint w x y = do  
    if isFixed c 
        then w
        else w { moveState = Hint (row, col) }
    where 
        row = rowByY y  
        col = colByX x 
        c = getCell (field w) row col 
        
-- info -----------------------------------------------------------------------
handleInfo :: World -> World 
handleInfo w =
    case gameState w of
        ShowInfo -> w { gameState = InProgress }
        _        -> w { gameState = ShowInfo }      

-- screen areas hit predicates ------------------------------------------------        
-------------------------------------------------------------------------------
fieldHit :: Float -> Float -> Bool
fieldHit x y = (x <= tlppX + 9 * cellSize)  && (y >= tlppY - 9 * cellSize)  &&
               (x >= tlppX)                 && (y <= tlppY) 

numpadHit :: Float -> Float -> Bool 
numpadHit x y = (x >= trppX - 3 * cellSize) && (y >= trppY - 4 * cellSize)  &&
                (x <= trppX)                && (y <= trppY)                

                
-- =============== UPDATE FUNCTION ============================================
-- increases total game time with each frap -----------------------------------
updateWorld :: Float -> World -> IO World
updateWorld dt w = 
    case gameState w of
       InProgress  -> return w'
       Error       -> return w'

       Finished    -> fixRec w
       _           -> return w
    where
        w' = w { totalTime = totalTime w + dt }
