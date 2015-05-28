module Main where

import Types
import Parser
import Interface

import Graphics.Gloss.Interface.IO.Game

-- =============== MAIN FUNCTION ==============================================
main :: IO()
main = do
    putStrLn "Welcome to Sudoku puzzle game" 
    interface  
    
-- graphics user interface ----------------------------------------------------                 
interface :: IO ()
interface = do

    w <- readWorld

    case gameState w of
        InProgress -> playIO display bgColor fps w 
                           drawWorld handleWorld updateWorld  
        _gs -> putStrLn "Incorrect input file"
    where
        windowSize = (floor winWidth, floor winHeight)
        windowOffset = (200, 200)
        display = InWindow "BEST SUDOKU EVAR" windowSize windowOffset
        bgColor = white
        fps = 60
