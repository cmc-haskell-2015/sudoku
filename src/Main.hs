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
    interface  
    
-- graphics user interface ----------------------------------------------------                 
interface :: IO ()
interface = do
    w <- readWorld
    case gameState w of
        InProgress -> play display bgColor fps w 
                           drawWorld handleWorld updateWorld       
        gs -> putStrLn "Incorrect input file"
    where
        windowSize = (floor winWidth, floor winHeight)
        windowOffset = (200, 200)
        display = InWindow "BEST SUDOKU EVAR" windowSize windowOffset
        bgColor = white
        fps = 60