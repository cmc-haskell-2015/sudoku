module Main where

import AuxFunc
import Types
import Game

import Data.Functor
import Data.Char
import Data.String 

import Graphics.Gloss.Interface.Pure.Game

-- =============== MAIN FUNCTION ==============================================
main :: IO()
main = do
    greetingsMsg
    playFromFile 
--  playGame (initField, Ok, InProgress) 
-- ============================================================================


-- function for field hard-coding ---------------------------------------------
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
                 

                 
                 
-- =============== MANAGE GAME ================================================ 
-- request file name ----------------------------------------------------------   
playFromFile :: IO ()
playFromFile = do
    print "Enter file name: "
    file <- getLine
    readGame file

-- read the game from file and start it ---------------------------------------
readGame :: FilePath -> IO ()
readGame file = do
    strList <- lines <$> readFile file
    let f = (createField strList)    
    if ((checkField f == True) && (checkFinished f == False)) then     
        playGame (f, Ok, InProgress)
    else
        print "Incorrect input puzzle"

-- transform file contents into field representation --------------------------
createField :: [String] -> Field
createField [] = []
createField (x:xs) = (parseString x) : (createField xs)

-- transform string into list of cells ----------------------------------------
parseString :: String -> [Cell]
parseString [] = []
parseString ('0':xs) = Empty : parseString xs
parseString (x:xs) | (isDigit x == True) 
                       = Fixed (ord x - ord '0') : parseString xs
                   | otherwise = parseString xs 


   
    