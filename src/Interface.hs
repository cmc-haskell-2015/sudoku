module Interface where

import Types
import Game
import Parser

import Data.Functor
import Data.Char
import Data.String
import Data.Monoid 

import Graphics.Gloss.Interface.Pure.Game

-- =============== AUXILIARY FUNCTIONS ========================================
winHeight :: Int -- game window height
winHeight = 540

winWidth :: Int -- game window width
winWidth = (winHeight + 3 * cellSize) + 100 -- the second item stands for the
                                            -- interval between field 
                                            -- and numberpad

tlppX = - div winWidth 2 -- top left pixel position (X)
tlppY = div winHeight 2  -- top left pixel position (Y)

trppX = div winWidth 2   -- top right pixel position (X)
trppY = div winHeight 2  -- top right pixel position (Y)

blppX = tlppX            -- bottom left pixel position (X)
blppY = - tlppY          -- bottom left pixel position (Y)

brppX = trppX            -- bottom right pixel position (X)
brppY = - trppY          -- bottom right pixel position (Y)

cellSize :: Int
cellSize = div winHeight 9  

tlcpX :: Int -- top left cell position (X)
tlcpX = (div cellSize 2) - (div winWidth 2)
 
tlcpY :: Int -- top left cell position (Y)
tlcpY = - (div cellSize 2) + (div winHeight 2)

trnpX :: Int -- top right number position (X)
trnpX = (div winWidth 2) - (div cellSize 2)
 
trnpY :: Int -- top right number position (Y)
trnpY = (div winHeight 2) - (div cellSize 2) 

digScl :: Float -- digits scale to fit the cells 
digScl = ((fromIntegral cellSize) / 50) * 0.3 

cellPosX :: Int -> Float -- cell X coordinate by its column index
cellPosX col = fromIntegral (tlcpX + cellSize * col)

cellPosY :: Int -> Float -- cell Y coordinate by its row index
cellPosY row = fromIntegral (tlcpY - cellSize * row)

numPosX :: Float -> Float -- number X coordinate according to cell X coordinate
numPosX posX = posX - fromIntegral (div cellSize 4)

numPosY :: Float -> Float -- number Y coordinate according to cell Y coordinate
numPosY posY = posY - fromIntegral (div cellSize 4)

rowByY :: Float -> Int -- cell row index by Y coordinate
rowByY y = 8 - floor ((y + (fromIntegral winHeight)/2) / 
                      (fromIntegral cellSize))

colByX :: Float -> Int -- cell column index by X coordinate
colByX x | x <= (fromIntegral(tlppX + 9 * cellSize)) 
               = floor ((x + (fromIntegral winWidth)/2) / 
                        (fromIntegral cellSize))
         |  otherwise = (-1)
         
numByXY :: Float -> Float -> Int -- chosen number on the numberpad     
numByXY x y = do
    let nrow = 2 - floor ((y - (fromIntegral winHeight)/2 + 
                          (fromIntegral (3 * cellSize))) / 
                              (fromIntegral cellSize))
    let ncol = floor ((x - (fromIntegral winWidth)/2 + 
                      (fromIntegral (3 * cellSize))) / 
                          (fromIntegral cellSize))        
    return_same (ncol + (3 * nrow) + 1)
    
-- =============== DRAWING FUCTIONS ===========================================
-- world ----------------------------------------------------------------------         
drawWorld :: World -> Picture
drawWorld (f, ms, Finished, t) = drawFinish <> drawFinishTime t
drawWorld (f, ms, ShowInfo, t) = drawInfo
                              <> drawTime t
drawWorld (f, ms, Error, t) = drawField f ms 
                        <> drawNumberpad 
                        <> drawMoveState ms 
                        <> drawInfoSuggest
                        <> drawTime t
                        <> drawError
                    
drawWorld (f, ms, gs, t) = drawField f ms 
                        <> drawNumberpad 
                        <> drawMoveState ms 
                        <> drawInfoSuggest
                        <> drawTime t

-- time -----------------------------------------------------------------------
drawTime :: Float -> Picture
drawTime t = translate (fromIntegral(tlppX + 9 * cellSize + 5)) 
                       (fromIntegral(brppY + 5))
                           (color (blue)
                               (scale (0.5 * digScl) (0.5 * digScl)
                                   (text("time:"))))
          <> translate (fromIntegral(tlppX + 10 * cellSize + 10)) 
                       (fromIntegral(brppY + 5))
                           (color (blue)
                               (scale (0.5 * digScl) (0.5 * digScl)
                                   (text((show min)++
                                         " min "++
                                         (show sec)++
                                         " sec"))))                                   
    where 
        s = floor t
        min = div s 60
        sec = mod s 60

drawFinishTime :: Float -> Picture
drawFinishTime t = translate (fromIntegral(-4 * cellSize)) 
                             (fromIntegral(0))
                       (color (blue)
                           (scale (digScl) (digScl)
                               (text("time:"))))
                <> translate (fromIntegral(- cellSize)) 
                             (fromIntegral(0))
                           (color (blue)
                               (scale (digScl) (digScl)
                                   (text((show min)++
                                         " min "++
                                         (show sec)++
                                         " sec"))))                                   
    where 
        s = floor t
        min = div s 60
        sec = mod s 60         
                        
-- info -----------------------------------------------------------------------
drawInfoSuggest :: Picture
drawInfoSuggest = translate (fromIntegral(tlppX + 9 * cellSize + 5)) 
                             (fromIntegral (brppY + 3* div cellSize 2)) 
                       (scale (0.4 * digScl) (0.4 * digScl) 
                           (text ("Press F1 to show Info")))

drawInfo :: Picture
drawInfo = drawHeadline
        <> drawHowToPlay 
        <> drawCloseInfo        

drawHeadline :: Picture
drawHeadline = translate (fromIntegral tlppX) 
                         (fromIntegral (tlppY - div cellSize 2))
                   (scale (digScl) (digScl)
                       (text "========================"))                   
            <> translate (fromIntegral (tlppX + 5 * cellSize)) 
                         (0.9 * fromIntegral (tlppY - cellSize))
                   (scale (1.2 * digScl) (1.2 * digScl)
                       (text "SUDOKU"))  
            <> translate (fromIntegral tlppX) 
                         (fromIntegral (tlppY - 2 * cellSize))
                   (scale (digScl) (digScl)
                       (text "========================"))  

drawHowToPlay :: Picture
drawHowToPlay = translate (fromIntegral(tlppX + div cellSize 2)) 
                          (fromIntegral (tlppY - 3 * cellSize)) 
                       (scale (0.5*digScl) (0.5*digScl) 
                           (text ("Select a cell that you want to " ++
                                  "(re)fill by mouse click.")))
             <> translate (fromIntegral(tlppX + div cellSize 2)) 
                          (fromIntegral (tlppY - div (7 * cellSize) 2)) 
                       (scale (0.5*digScl) (0.5*digScl) 
                             (text ("Then choose a digit " ++ 
                                    "from the numberpad.")))
             <> translate (fromIntegral(tlppX + div cellSize 2)) 
                          (fromIntegral (tlppY - 9 * div cellSize 2)) 
                       (scale (0.4*digScl) (0.4*digScl) 
                             (text ("Right mouse click (or press 'h') " ++ 
                                    "on a cell will show possible numbers.")))                                    
             <> translate (fromIntegral(tlppX + div cellSize 2)) 
                          (fromIntegral (tlppY - 11 * div cellSize 2)) 
                       (scale (0.35*digScl) (0.35*digScl) 
                             (text ("You can use either keyboard " ++ 
                                    "(arrows + numbers + space) or the given numpad.")))
                                    
drawCloseInfo :: Picture
drawCloseInfo = translate (fromIntegral (blppX + div cellSize 2))
                          (fromIntegral (blppY + 5))
                    (scale (0.5*digScl) (0.5*digScl)
                        (text "Press F1 to close Info"))                    
                                    
-- game over ------------------------------------------------------------------
drawFinish :: Picture
drawFinish = translate (fromIntegral (-5 * div cellSize 2)) 
                       (fromIntegral (2 * cellSize)) 
                                  (scale (1.5 * digScl) (1.5 * digScl) 
                                      (text "YOU WIN"))

-- selected cell overlay ------------------------------------------------------                                      
drawSelectedCell :: Int -> Int -> Picture
drawSelectedCell srow scol = pictures [color green 
                                 (drawCell (Empty) 
                                           (cellPosX scol) 
                                           (cellPosY srow) 
                                           (cellSize) 
                                           (cellSize))    ]       

-- error ----------------------------------------------------------------------
drawError :: Picture
drawError = translate (fromIntegral(tlppX + 9 * cellSize + 5))
                      (fromIntegral (0))
                (color (dark red)
                    (scale (0.45 * digScl) (0.45 * digScl)
                        (text "Smth wrong")) )                          
         <> translate (fromIntegral(tlppX + 9 * cellSize + 5))
                      (fromIntegral (-div cellSize 2))
                (color (dark red) 
                    (scale (0.45 * digScl) (0.45 * digScl)
                        (text "check for errors")))                                            
                                           
-- hint -----------------------------------------------------------------------
drawHint :: Field -> Int -> Int -> Picture
drawHint f hrow hcol = do
    let hint = getHint f hrow hcol
    if (hint /= []) then do
        drawSelectedCell hrow hcol
        <> translate (fromIntegral(tlppX + 9 * cellSize + 5))
                     (fromIntegral (0))
               (color (dark green)
                   (scale (0.45 * digScl) (0.45 * digScl)
                       (text "Possible for selected cell:")) )                          
        <> translate (fromIntegral(tlppX + 9 * cellSize + 5))
                     (fromIntegral (-div cellSize 2))
               (color (dark green) 
                   (scale (0.45 * digScl) (0.45 * digScl)
                       (text (show (getHint f hrow hcol)))))
    else do
        drawSelectedCell hrow hcol
        <> drawError    
    
                                           
-- move results ---------------------------------------------------------------                                      
drawMoveState :: MoveState -> Picture
drawMoveState ms = translate (fromIntegral(tlppX + 9 * cellSize + 5)) 
                             (fromIntegral (-2 * cellSize)) 
                       (scale (0.4 * digScl) (0.4 * digScl) 
                           (text (show ms)))

-- field ----------------------------------------------------------------------                           
drawField :: Field -> MoveState -> Picture
drawField f (Selected (srow, scol)) = drawAllCells f 
                                   <> drawLines
                                   <> drawGrid
                                   <> drawSelectedCell srow scol
                                   
drawField f (Hint (hrow, hcol)) = drawAllCells f
                               <> drawLines                                   
                               <> drawGrid
                               <> drawHint f hrow hcol
                               
drawField f _ = drawAllCells f 
             <> drawLines
             <> drawGrid
-- grid ----------------------------------------------------------------------- 
drawGrid :: Picture
drawGrid = pictures [(drawCell 
                            (Empty) 
                            (cellPosX col) 
                            (cellPosY row) 
                            (cellSize) (cellSize)) 
                           
                           |
                                row <- [0,1 ..8],  
                                col <- [0,1 ..8]] 
                                
-- additional lines between 3x3 squares for comfort ---------------------------                      
drawLines :: Picture
drawLines = line [(fromIntegral (tlppX + (3 * cellSize) + 1), 
                       fromIntegral(tlppY)), 
                  (fromIntegral (tlppX + (3 * cellSize) + 1), 
                       fromIntegral (tlppY - (9 * cellSize)))]
         <> line [(fromIntegral (tlppX +  (6 * cellSize) + 1), 
                       fromIntegral(tlppY)), 
                  (fromIntegral (tlppX + (6 * cellSize) + 1),
                      fromIntegral( tlppY - (9 * cellSize)))]             
         <> line [(fromIntegral(tlppX), 
                      fromIntegral (tlppY - (3 * cellSize) + 1)), 
                  (fromIntegral (tlppX + (9 * cellSize)), 
                      fromIntegral(tlppY - (3 * cellSize) + 1))]
         <> line [(fromIntegral(tlppX), 
                      fromIntegral(tlppY - (6 * cellSize) + 1)), 
                  (fromIntegral(tlppX + (9 * cellSize)), 
                      fromIntegral(tlppY - (6 * cellSize) + 1))]  
         <> line [(fromIntegral (tlppX + (3 * cellSize) + 2), 
                      fromIntegral(tlppY)), 
                  (fromIntegral (tlppX + (3 * cellSize) + 2), 
                      fromIntegral (tlppY - (9 * cellSize)))]
         <> line [(fromIntegral (tlppX +  (6 * cellSize) + 2), 
                       fromIntegral(tlppY)), 
                  (fromIntegral (tlppX + (6 * cellSize) + 2), 
                       fromIntegral( tlppY - (9 * cellSize)))]             
         <> line [(fromIntegral(tlppX), 
                       fromIntegral (tlppY - (3 * cellSize) + 2)), 
                  (fromIntegral (tlppX + (9 * cellSize)), 
                       fromIntegral(tlppY - (3 * cellSize) + 2))]
         <> line [(fromIntegral(tlppX), 
                       fromIntegral(tlppY - (6 * cellSize) + 2)), 
                  (fromIntegral(tlppX + (9 * cellSize)), 
                       fromIntegral(tlppY - (6 * cellSize) + 2))]              
              
              
-- single cell ----------------------------------------------------------------
drawCell :: Cell -> Float -> Float -> Int -> Int -> Picture
drawCell Empty posX posY sizeX sizeY = 
    translate (posX) (posY) 
        (rectangleWire (fromIntegral sizeX) (fromIntegral sizeY))
        
drawCell (Filled n) posX posY sizeX sizeY = 
    pictures [translate (posX) (posY)
                  (rectangleWire (fromIntegral sizeX) (fromIntegral sizeY)),
              translate (numPosX posX) (numPosY posY) 
                  (scale (digScl) (digScl) (text (show n)))]   
                  
drawCell (Fixed n) posX posY sizeX sizeY = 
    pictures [color (dark white) (translate (posX) (posY)
                  (rectangleSolid (fromIntegral sizeX) (fromIntegral sizeY))),
              translate (numPosX posX) (numPosY posY) 
                  (scale (digScl) (digScl) (text (show n)))] 
                  
-- all the cells in the field -------------------------------------------------
drawAllCells :: Field -> Picture
drawAllCells f = pictures [(drawCell 
                            (getCell f row col) 
                            (cellPosX col) 
                            (cellPosY row) 
                            (cellSize) (cellSize)) 
                           
                           |
                                row <- [0,1 ..8],  
                                col <- [0,1 ..8]]               
-- numberpad ------------------------------------------------------------------                  
drawNumberpad :: Picture
drawNumberpad = pictures [
                  drawCell (Filled 1) 
                      (fromIntegral(trnpX - 2 * cellSize)) 
                      (fromIntegral trnpY) 
                      (cellSize) (cellSize),
                  drawCell (Filled 2) 
                      (fromIntegral(trnpX - 1 * cellSize)) 
                      (fromIntegral trnpY)
                      (cellSize) (cellSize),
                  drawCell (Filled 3) 
                      (fromIntegral trnpX) 
                      (fromIntegral trnpY) 
                      (cellSize) (cellSize),
                  drawCell (Filled 4) 
                      (fromIntegral(trnpX - 2 * cellSize)) 
                      (fromIntegral(trnpY - 1 * cellSize)) 
                      (cellSize) (cellSize),
                  drawCell (Filled 5) 
                      (fromIntegral(trnpX - 1 * cellSize)) 
                      (fromIntegral(trnpY - 1 * cellSize)) 
                      (cellSize) (cellSize),
                  drawCell (Filled 6) 
                      (fromIntegral trnpX) 
                      (fromIntegral(trnpY - 1 * cellSize)) 
                      (cellSize) (cellSize),
                  drawCell (Filled 7) 
                      (fromIntegral(trnpX - 2 * cellSize)) 
                      (fromIntegral(trnpY - 2 * cellSize)) 
                      (cellSize) (cellSize),
                  drawCell (Filled 8) 
                      (fromIntegral(trnpX - 1 * cellSize)) 
                      (fromIntegral(trnpY - 2 * cellSize)) 
                      (cellSize) (cellSize),
                  drawCell (Filled 9)
                      (fromIntegral trnpX)
                      (fromIntegral(trnpY - 2 * cellSize)) 
                      (cellSize) (cellSize),
                  drawClearButton                          ]

drawClearButton :: Picture
drawClearButton = drawCell (Empty) 
                      (fromIntegral (trnpX - 1 * cellSize))
                      (fromIntegral (trnpY - 3 * cellSize))
                      (3 * cellSize) (cellSize)
               <> translate 
                      (fromIntegral ((trnpX - 1 * cellSize)-(cellSize)))
                      (fromIntegral ((trnpY - 3 * cellSize)-(div cellSize 4)))                
                          (scale (0.8*digScl) (0.8*digScl) (text ("CLEAR")))                
                  
-- =============== EVENTS HANDLER =============================================
-- select cell in the field ---------------------------------------------------
handleSelect :: World -> Float -> Float -> World
handleSelect (f, ms, gs, t) x y = do
    let row = rowByY(y) 
    let col = colByX(x)
    let c = getCell f row col 
    if (c == (Fixed 0)) then 
        return_same (f, ms, gs, t)
    else 
        return_same (f, Selected (row, col), gs, t)

-- make move ------------------------------------------------------------------    
handleMove :: World -> Float -> Float -> World
handleMove (f, Selected (row, col), gs, t) x y = do
    let num = numByXY x y
    if (num <= 9) then do
        makeMove (f, Selected (row, col), gs, t) row col num   
    else do 
        clearCell (f, Selected (row, col), gs, t) row col
handleMove (f, Hint (row, col), gs, t) x y = do
    let num = numByXY x y
    if (num <= 9) then do
        makeMove (f, Selected (row, col), gs, t) row col num   
    else do 
        clearCell (f, Selected (row, col), gs, t) row col         
handleMove w _ _ = w

-- show hint ------------------------------------------------------------------
handleHint :: World -> Float -> Float -> World
handleHint (f,ms,gs,t) x y = do 
    let row = rowByY(y) 
    let col = colByX(x)
    let c = getCell f row col 
    if (c == (Fixed 0)) then 
        return_same (f, ms, gs, t)
    else 
        return_same (f, Hint (row, col), gs, t)

-- info -----------------------------------------------------------------------
handleInfo :: World -> World 
handleInfo (f, ms, ShowInfo, t) = (f, ms, InProgress, t)
handleInfo (f, ms, gs, t) = (f, ms, ShowInfo, t)
        
-- main handler function ------------------------------------------------------
-- defines what action to do according to mouse click coordinates -------------    
handleWorld :: Event -> World -> World
handleWorld _ (f, ms, Finished, t) = (f, ms, Finished, t)
handleWorld (EventKey (MouseButton LeftButton) Down _ (x, y)) w = do
    if (fieldHit x y)    
    then do
        handleSelect w x y
    else do 
        if (numpadHit x y) 
        then do
            handleMove w x y
        else do
            return_same w
handleWorld (EventKey (MouseButton RightButton) Down _ (x, y)) w = do
    if (fieldHit x y)    
    then 
        handleHint w x y
    else 
        return_same w   
handleWorld (EventKey (SpecialKey KeyF1) Down _ _) w = handleInfo w

handleWorld (EventKey (SpecialKey KeySpace) Down _ _) 
            (f, Selected(row,col), gs, t) = 
                clearCell (f, Selected (row, col), gs, t) row col 
handleWorld (EventKey (SpecialKey KeySpace) Down _ _) 
            (f, Hint(row,col), gs, t) = 
                clearCell (f, Hint (row, col), gs, t) row col 
  
handleWorld (EventKey (Char 'h') Down _ _)
            (f, Selected(row,col), gs, t) = 
                (f, Hint(row,col), gs, t) 
  
handleWorld (EventKey (Char c) Down _ _)
            (f, Selected(row,col), gs, t) = 
                handleChar c (f, Selected(row,col), gs, t)
handleWorld (EventKey (Char c) Down _ _)
            (f, Hint(row,col), gs, t) = 
                handleChar c (f, Hint(row,col), gs, t)

handleWorld (EventKey (SpecialKey KeyRight) Down _ _) w = 
                handleRight w  
handleWorld (EventKey (SpecialKey KeyLeft) Down _ _) w = 
                handleLeft w 
handleWorld (EventKey (SpecialKey KeyUp) Down _ _) w = 
                handleUp w 
handleWorld (EventKey (SpecialKey KeyDown) Down _ _) w = 
                handleDown w 
              
handleWorld _ w = w    

-------------------------------------------------------------------------------
handleChar :: Char -> World -> World
handleChar '0' w = w
handleChar c (f,Selected(row,col),gs,t) = do 
    if (isDigit c == True) then 
        makeMove (f,Selected(row,col),gs,t) row col (ord c - ord '0')
    else return_same (f,Selected(row,col),gs,t)  
handleChar c (f,Hint(row,col),gs,t) = do 
    if (isDigit c == True) then 
        makeMove (f,Selected(row,col),gs,t) row col (ord c - ord '0')
    else return_same (f,Hint(row,col),gs,t)  
handleChar _ w = w   
-------------------------------------------------------------------------------
handleRight :: World -> World
handleRight (f,Selected(row0,col0),gs,t) = do
    let (row1, col1) = closestRight f row0 col0 
    if (col1 == 9) then 
        return_same (f, Selected(row0,col0),gs,t)
    else
        return_same (f, Selected(row1,col1),gs,t) 
handleRight (f,Hint(row0,col0),gs,t) = do
    let (row1, col1) = closestRight f row0 col0 
    if (col1 == 9) then 
        return_same (f, Selected(row0,col0),gs,t)
    else
        return_same (f, Selected(row1,col1),gs,t)
handleRight (f, ms, gs, t) = (f, Selected(0,0), gs, t)       
-------------------------------------------------------------------------------
closestRight :: Field -> Int -> Int -> (Int, Int)
closestRight f row col | ((col == 8) && (getCell f row col == (Fixed 0))) 
                             = (row,9)
                       | ((col <= 7) && (getCell f row (col+1) == (Fixed 0))) 
                             = closestRight f row (col+1) 
                       | otherwise = (row, col+1)
-------------------------------------------------------------------------------
handleLeft :: World -> World
handleLeft (f,Selected(row0,col0),gs,t) = do
    let (row1, col1) = closestLeft f row0 col0 
    if (col1 == -1) then 
        return_same (f, Selected(row0,col0),gs,t)
    else
        return_same (f, Selected(row1,col1),gs,t) 
handleLeft (f,Hint(row0,col0),gs,t) = do
    let (row1, col1) = closestLeft f row0 col0 
    if (col1 == -1) then 
        return_same (f, Selected(row0,col0),gs,t)
    else
        return_same (f, Selected(row1,col1),gs,t)
handleLeft (f, ms, gs, t) = (f, Selected(0,0), gs, t)       
-------------------------------------------------------------------------------
closestLeft :: Field -> Int -> Int -> (Int, Int)
closestLeft f row col | ((col == 0) && (getCell f row col == (Fixed 0))) 
                             = (row,-1)
                       | ((col >= 1) && (getCell f row (col-1) == (Fixed 0))) 
                             = closestLeft f row (col-1) 
                       | otherwise = (row, col-1)          
-------------------------------------------------------------------------------
handleUp :: World -> World
handleUp (f,Selected(row0,col0),gs,t) = do
    let (row1, col1) = closestUp f row0 col0 
    if (row1 == -1) then 
        return_same (f, Selected(row0,col0),gs,t)
    else
        return_same (f, Selected(row1,col1),gs,t) 
handleUp (f,Hint(row0,col0),gs,t) = do
    let (row1, col1) = closestUp f row0 col0 
    if (row1 == -1) then 
        return_same (f, Selected(row0,col0),gs,t)
    else
        return_same (f, Selected(row1,col1),gs,t)
handleUp (f, ms, gs, t) = (f, Selected(0,0), gs, t)       
-------------------------------------------------------------------------------
closestUp :: Field -> Int -> Int -> (Int, Int)
closestUp f row col    | ((row == 0) && (getCell f row col == (Fixed 0))) 
                             = (-1,col)
                       | ((row >= 1) && (getCell f (row-1) (col) == (Fixed 0))) 
                             = closestUp f (row-1) (col) 
                       | otherwise = (row-1, col)          
-------------------------------------------------------------------------------
handleDown :: World -> World
handleDown (f,Selected(row0,col0),gs,t) = do
    let (row1, col1) = closestDown f row0 col0 
    if (row1 == 9) then 
        return_same (f, Selected(row0,col0),gs,t)
    else
        return_same (f, Selected(row1,col1),gs,t) 
handleDown (f,Hint(row0,col0),gs,t) = do
    let (row1, col1) = closestDown f row0 col0 
    if (row1 == 9) then 
        return_same (f, Selected(row0,col0),gs,t)
    else
        return_same (f, Selected(row1,col1),gs,t)
handleDown (f, ms, gs, t) = (f, Selected(0,0), gs, t)       
-------------------------------------------------------------------------------
closestDown :: Field -> Int -> Int -> (Int, Int)
closestDown f row col  | ((row == 8) && (getCell f row col == (Fixed 0))) 
                             = (9,col)
                       | ((row <= 7) && (getCell f (row+1) (col) == (Fixed 0))) 
                             = closestDown f (row+1) (col) 
                       | otherwise = (row+1, col)                                                      
-------------------------------------------------------------------------------
fieldHit :: Float -> Float -> Bool
fieldHit x y = (x <= (fromIntegral(tlppX + 9 * cellSize))) &&
               (y >= (fromIntegral(tlppY - 9* cellSize)))  &&
               (x >= (fromIntegral tlppX))                 && 
               (y <= (fromIntegral tlppY)) 

numpadHit :: Float -> Float -> Bool 
numpadHit x y = (x >= (fromIntegral(trppX - 3 * cellSize))) && 
                (y >= (fromIntegral(trppY - 4 * cellSize))) &&
                (x <= (fromIntegral trppX))                 && 
                (y <= fromIntegral trppY)                

-- =============== UPDATE FUNCTION ============================================
-- do not need it -------------------------------------------------------------
updateWorld :: Float -> World -> World
updateWorld time (f,ms,InProgress,t) = (f,ms,InProgress,t+time)
updateWorld time (f,ms,Error,t) = (f,ms,Error,t+time)
updateWorld _ w = w                    