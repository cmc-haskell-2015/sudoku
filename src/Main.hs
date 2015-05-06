module Main where

import Types
import Game
import Parser
import Interface

import Data.Functor
import Data.Char
import Data.String
import Data.Monoid 

import Graphics.Gloss.Interface.Pure.Game


-- =============== MAIN FUNCTION ==============================================
main :: IO()
main = do
    putStrLn "Welcome to Sudoku puzzle game" 
    interface  -- start with graphics
    -- console -- start in console    
-- ============================================================================

-- start console game ---------------------------------------------------------
console :: IO ()
console = do 
    (f, ms, gs, t) <- readWorld
    if (gs == InProgress) then 
        playGame (f, ms, gs, t)
    else
        print gs
                   
-- start game with graphics ---------------------------------------------------                 
interface :: IO ()
interface = do
    (f, ms, gs, t) <- readWorld
    if (gs == InProgress) then do 
        play display bgColor fps (f,ms,gs,t) drawWorld handleWorld updateWorld       
    else do
        print gs
    where    
        windowSize = (winWidth, winHeight)
        windowOffset = (200, 200)
        display = InWindow "BEST SUDOKU EVAR" windowSize windowOffset
        bgColor = white
        fps = 60     
