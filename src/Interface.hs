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

cellSize :: Int
cellSize = div winHeight 9  

tlcpX :: Int -- top left cell position (X)
tlcpX = (div cellSize 2) - (div winWidth 2)
 
tlcpY :: Int -- top left cell position (Y)
tlcpY = - (div cellSize 2) + (div winHeight 2)

trnpX :: Int-- top right number position (X)
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
drawWorld (f, ms, Finished) = drawFinish
drawWorld (f, ms, gs) = drawField f ms 
                     <> drawNumberpad 
                     <> drawMoveState ms 
                     <> drawInfo
                     
-- info -----------------------------------------------------------------------
drawInfo :: Picture
drawInfo = translate (fromIntegral(tlppX + 9 * cellSize + 5)) 
                             (fromIntegral (cellSize)) 
                       (scale (0.4 * digScl) (0.4 * digScl) 
                           (text ("Select a cell that you want")))
        <> translate (fromIntegral(tlppX + 9 * cellSize + 5)) 
                             (fromIntegral (div cellSize 2)) 
                       (scale (0.4 * digScl) (0.4 * digScl)                            
                             (text ("to (re)fill by mouse click")))
        <> translate (fromIntegral(tlppX + 9 * cellSize + 5)) 
                             (0) 
                       (scale (0.4 * digScl) (0.4 * digScl) 
                             (text ("then choose a digit")))
        <> translate (fromIntegral(tlppX + 9 * cellSize + 5)) 
                             (fromIntegral (-div cellSize 2)) 
                       (scale (0.4 * digScl) (0.4 * digScl)                              
                             (text ("from the numberpad")))

-- game over ------------------------------------------------------------------
drawFinish :: Picture
drawFinish = translate (fromIntegral (- div winWidth 4)) 0 
                                  (scale (2 * digScl) (2 * digScl) 
                                      (text "YOU WIN"))

-- selected cell overlay ------------------------------------------------------                                      
drawSelectedCell :: Int -> Int -> Picture
drawSelectedCell srow scol = pictures [color green 
                                 (drawCell (Empty) 
                                           (cellPosX scol) 
                                           (cellPosY srow) 
                                           (cellSize) 
                                           (cellSize))    ]       
                                           
-- move results in bottom right corner ----------------------------------------                                      
drawMoveState :: MoveState -> Picture
drawMoveState ms = translate (fromIntegral(tlppX + 9 * cellSize + 5)) 
                             (fromIntegral (-3 * cellSize)) 
                       (scale (0.4 * digScl) (0.4 * digScl) 
                           (text (show ms)))

-- field ----------------------------------------------------------------------                           
drawField :: Field -> MoveState -> Picture
drawField f (Selected (srow, scol)) = drawAllCells f 
                                   <> drawSelectedCell srow scol
                                   <> drawLines
drawField f _ = drawAllCells f 
             <> drawLines
                        
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
                      (cellSize) (cellSize)                ]
               
                  
-- =============== EVENTS HANDLER =============================================
-- select cell in the field ---------------------------------------------------
handleSelect :: World -> Float -> Float -> World
handleSelect (f, ms, gs) x y = do
    let row = rowByY(y) 
    let col = colByX(x)
    return_same (f, Selected (row, col), gs)

-- make move ------------------------------------------------------------------    
handleMove :: World -> Float -> Float -> World
handleMove (f, Selected (row, col), gs) x y = do
    let num = numByXY x y
    makeMove (f, Selected (row, col), gs) row col num   
handleMove w _ _ = w

-- main handler function ------------------------------------------------------
-- defines what action to do according to mouse click coordinates -------------    
handleWorld :: Event -> World -> World
handleWorld _ (f, ms, Finished) = (f, ms, Finished)
handleWorld (EventKey (MouseButton LeftButton) Down _ (x, y)) w = do
    if (x <= (fromIntegral(tlppX + 9 * cellSize))) 
    then do
        handleSelect w x y
    else do 
    if ((x >= (fromIntegral(trppX - 3 * cellSize))) && 
        (y >= (fromIntegral(trppY - 3 * cellSize)))) 
    then do
        handleMove w x y
    else do
        return_same w
handleWorld _ w = w    


-- =============== UPDATE FUNCTION ============================================
-- do not need it -------------------------------------------------------------
updateWorld :: Float -> World -> World
updateWorld _ = id                    