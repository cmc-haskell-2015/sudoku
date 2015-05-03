module Parser where

import Types
import Game

import Data.Functor
import Data.Char
import Data.String

readWorld :: IO World
readWorld = do
    print "Enter file name: "
    file <- getLine
    readFromFile file
--  readFromFile "sud.txt"

-- read the game from file and start it ---------------------------------------
readFromFile :: FilePath -> IO World
readFromFile file = do
    strList <- lines <$> readFile file
    let f = (createField strList)    
    if ((checkField f == True) && (checkFinished f == False)) then     
        return (f, Ok, InProgress)
    else
        return ([], Ok, Error)

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
initWorld = (initField, Ok, InProgress)                   