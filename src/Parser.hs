module Parser where

import Types
import Game

import Data.Functor
import Data.Char

import System.IO

-- read puzzle conditions from file -------------------------------------------
readWorld :: IO World
readWorld = do
    putStr "Enter file name: "
    hFlush stdout
    file <- getLine
    readFromFile file

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
createField (x : xs) = (parseString x) : (createField xs)

parseString :: String -> [Cell]
parseString [] = []
parseString ('0' : xs) = Empty : parseString xs
parseString (x : xs) 
    | isDigit x = Fixed (ord x - ord '0') : parseString xs
    | otherwise = parseString xs 
