module Types where

-- CELL -----------------------------------------------------------------------
data Cell = Fixed Int | Filled Int | Empty deriving (Eq)

isFixed :: Cell -> Bool
isFixed (Fixed _) = True
isFixed _ = False

-- FIELD ----------------------------------------------------------------------  
type Field = [[Cell]]

-- MOVE -----------------------------------------------------------------------
data MoveState = Ok | ErrFixed | ErrImpossible | 
                 Selected (Int, Int) | Hint (Int, Int)
                 

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
data GameState = Finished | InProgress | ShowInfo | Result | Error deriving Eq

-- WORLD ----------------------------------------------------------------------    
data World = World
    { field     :: Field      -- ^ sudoku grid
    , moveState :: MoveState  -- ^ last move state (results)
    , gameState :: GameState  -- ^ global game state
    , totalTime :: Float      -- ^ total time spent on the puzzle
    }
