module Main where

import Types
import Game
import Parser
import Interface

import Data.Functor
import Data.Char
import Data.String
import Data.Monoid 

import Graphics.Gloss.Interface.IO.Game

instance Monoid b => Monoid (IO b) where
    mempty = return mempty

    mappend io1 io2 = do
        a1 <- io1
        a2 <- io2
        return (mappend a1 a2)
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
        gs -> putStrLn "Incorrect input file"
    where
        windowSize = (floor winWidth, floor winHeight)
        windowOffset = (200, 200)
        display = InWindow "BEST SUDOKU EVAR" windowSize windowOffset
        bgColor = white
        fps = 60