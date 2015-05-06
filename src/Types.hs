module Types where

-- CELL -----------------------------------------------------------------------
data Cell = Fixed Int | Filled Int | Empty

instance Show Cell where
    show (Empty)    = "o"
    show (Fixed n)  = show n
    show (Filled n) = show n

isFixed :: Cell -> Bool
isFixed (Fixed _) = True
isFixed _ = False

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
data MoveState = Ok | ErrFixed | ErrImpossible | 
                 Selected (Int,Int) | Hint (Int,Int)
instance Show MoveState where
    show Ok            = "Successful move"
    show ErrImpossible = "Impossible move: intersection"
    show ErrFixed      = "Can not refill fixed cell"
    show (Selected _)  = ""
    show (Hint _)      = "" 

getSelected :: MoveState -> Maybe (Int, Int)
getSelected (Selected s) = Just s
getSelected (Hint     s) = Just s
getSelected _ = Nothing

isSelected :: MoveState -> Bool
isSelected ms =
  case getSelected ms of
    Nothing -> False
    Just _  -> True

-- GAME -----------------------------------------------------------------------    
data GameState = Finished | InProgress | ShowInfo | Error deriving Eq
instance Show GameState where
    show Finished   = "You win"
    show InProgress = "The game is not finished yet"
    show Error      = "Something went wrong :c"
    show ShowInfo   = "Info" 

-- WORLD ----------------------------------------------------------------------    
data World = World
  { field     :: Field      -- ^ sudoku grid
  , moveState :: MoveState  -- ^ ???
  , gameState :: GameState  -- ^ global game state
  , totalTime :: Float      -- ^ total time spent on the puzzle
  }


