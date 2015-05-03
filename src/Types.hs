module Types where

-- CELL -----------------------------------------------------------------------
data Cell = Fixed Int | Filled Int | Empty

instance Show Cell where
    show (Empty)    = "o"
    show (Fixed n)  = show n
    show (Filled n) = show n

instance Eq Cell where
    (Fixed n1)  == (Fixed 0)   = True
    (Filled n1) == (Filled 0)  = True 
    (Fixed n1)  == (Fixed n2)  = (n1 == n2)
    (Filled n1) == (Filled n2) = (n1 == n2)
    (Empty)     == (Empty)     = True
    _           == _           = False  
        
-- FIELD ----------------------------------------------------------------------  
type Field = [[Cell]]

printField :: Field -> IO()
printField f = do 
    print "---------------------------------------"
    print "Current field state: "
    print "---------------------------------------"
    printField_r f
    print "---------------------------------------"

printField_r :: Field -> IO()    
printField_r [] = return ()
printField_r (x:xs) = do 
    print x
    printField_r xs

-- MOVE -----------------------------------------------------------------------
data MoveState = Ok | ErrFixed | ErrImpossible | Selected (Int,Int)
instance Show MoveState where
    show Ok            = "Successful move"
    show ErrImpossible = "Impossible move: intersection"
    show ErrFixed      = "Can not refill fixed cell"
    show (Selected _)  = ""
    
-- GAME -----------------------------------------------------------------------    
data GameState = Finished | InProgress | Error deriving Eq
instance Show GameState where
    show Finished   = "You win"
    show InProgress = "The game is not finished yet"
    show Error      = "Something went wrong :c"
    
-- WORLD ----------------------------------------------------------------------    
type World = (Field, MoveState, GameState)