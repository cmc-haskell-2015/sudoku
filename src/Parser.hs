module Parser where

import Types
import Game

import Data.Functor
import Data.Char
import Data.String
import Data.Monoid

import System.IO
import System.Directory

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

-- write time result to the file "Results.txt"-----------------------------------
fixRec :: World -> IO World
fixRec w = saveTime w >> return w { moveState = Ok, gameState = Result } 

saveTime :: World -> IO ()
saveTime (World f ms gs t) = updateFile t

writeToFile :: [Int] -> IO ()
writeToFile [] = appendFile "../Results.txt" ""
writeToFile (x:xs) = (appendFile "../Results.txt" 
                        $ (show x) ++ "\n") >> writeToFile xs

createFile :: IO ()
createFile = do 
    check <- doesFileExist "../Results.txt" 
    if check then return () else
        writeFile "../Results.txt" "" 

strToInt :: [String] -> [Int]                       
strToInt = map read
 
updateFile :: Float -> IO ()
updateFile curtime = do
    createFile
    handle <- openFile "../Results.txt" ReadMode
    oldRecArray  <- hGetContents handle
    putStrLn oldRecArray
    writeFile "../Results.txt" ""
    hClose handle
    writeToFile $ addRecord curRec $ strToInt (lines oldRecArray)
            where 
                curRec = floor curtime

-- array process ----------------------------------------------------------------
addRecord :: (Ord a) => a -> [a] -> [a]
addRecord x list = if elemInArray x list then list 
                        else insertInList x list

insertInList :: (Ord a) => a-> [a] -> [a]
insertInList num [] = [num]
insertInList num (x:xs) | (num < x) = num : x : xs
                        | (num == x) = num : x : xs
                        | otherwise = x : (insertInList num xs)

elemInArray :: (Eq a) => a -> [a] -> Bool
elemInArray _ [] = False
elemInArray elemnt (x:xs) = if (elemnt == x) then True 
                                else elemInArray elemnt xs

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
