module Records where

import Types

import System.IO
import System.Directory


-- write time result to the file "Results.txt"-----------------------------------
fixRec :: World -> IO World
fixRec w = saveTime w >> return w { moveState = Ok, gameState = Result } 

saveTime :: World -> IO ()
saveTime w = updateFile (totalTime w)

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

