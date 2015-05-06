module Parser where

import Types
import Game

import Data.Functor
import Data.Char
import Data.String

import System.IO

readWorld :: IO World
readWorld = do
    putStr "Enter file name: "
    hFlush stdout
    file <- getLine
    readFromFile file

-- read the game from file and start it ---------------------------------------
readFromFile :: FilePath -> IO World
readFromFile file = do
    strList <- lines <$> readFile file
    let f = createField strList
    if checkField f && not (checkFinished f) then     
        return (World f Ok InProgress 0)
    else
        return (World [] Ok Error 0)

-- transform file contents into field representation --------------------------
createField :: [String] -> Field
createField [] = []
createField (x:xs) = (parseString x) : (createField xs)

parseString :: String -> [Cell]
parseString [] = []
parseString ('0':xs) = Empty : parseString xs
parseString (x:xs) | (isDigit x == True) 
                       = Fixed (ord x - ord '0') : parseString xs
                   | otherwise = parseString xs 
                   
-- functions for world hard-coding --------------------------------------------
initField :: Field
initField = [[Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty], 
             [Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty], 
             [Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty], 
             [Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty], 
             [Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty], 
             [Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty], 
             [Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty], 
             [Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty], 
             [Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty]]

initWorld :: World
initWorld = World initField Ok InProgress 0
