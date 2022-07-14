module Lib where

import qualified Data.Set as S
import Control.Monad.State
import Control.Monad.Except


data Player = Circle | Cross | NoPlayer deriving (Eq)
instance Show Player where
    show Circle = "O"
    show Cross = "X"
    show NoPlayer = " "

data Position =
    TopLeft     | Top    | TopRight
    |CenterLeft | Center | CenterRight
    |BottomLeft | Bottom | BottomRight
    deriving (Eq, Ord, Show)

-- A move consist of a player and a position
newtype Move = Move {
    getMove :: (Player, Position)
} deriving (Show)

-- Make moves a stack to do easy take back one step and to push a move
newtype Moves = Moves {
    getMoves :: [Move]
}

somefunc moves = concatMap (\x -> show (lookupPosition x moves)) positions

lookupPosition :: Position -> Moves -> Player
lookupPosition _ (Moves []) = NoPlayer
lookupPosition pos (Moves (Move (player, position):xs)) =
    if pos == position
        then player
        else lookupPosition pos (Moves xs)

splitAt3s :: [a]-> [[a]]
splitAt3s [] = []
splitAt3s (x:x':x'':xs) = [x,x',x''] : splitAt3s xs
splitAt3s (x:x':xs) = [x,x'] : splitAt3s xs
splitAt3s (x:xs) = [x] : splitAt3s xs

positions = [  TopLeft, Top, TopRight
            , CenterLeft, Center, CenterRight
            , BottomLeft, Bottom, BottomRight]

addRowSplit :: String -> String
addRowSplit (x:xs) = '|' : x : addRowSplit xs
addRowSplit [] = ['|']

addColumnSplit :: [String] -> [String]
addColumnSplit (x:xs) = "-------" : x : addColumnSplit xs
addColumnSplit [] = ["-------"]

instance Show Moves where
    show moves = unlines $ 
        addColumnSplit (
            addRowSplit <$> 
            splitAt3s (concatMap (\x -> show (lookupPosition x moves)) positions))



-- Some helper functions

-- Gets all occupied spaces
getOccupied :: [Move] -> [Position]
getOccupied moves = snd <$> (getMove <$> moves)

-- Gets the last player that moved
getLastPlayer :: [Move] -> Player
getLastPlayer [] = NoPlayer
getLastPlayer (x:_) = let Move y = x in fst y

-- Check if there is a winner
whoWon :: [Move] -> Maybe Player
whoWon moves
    | circleWin = Just Circle
    | crossWin = Just Cross
    | otherwise = Nothing
    where
        circleMoves = S.fromList (snd <$> filter (\(player, pos) -> player == Circle) (getMove <$> moves))
        crossMoves = S.fromList (snd <$> filter (\(player, pos) -> player == Cross) (getMove <$> moves))
        circleWin = foldr (\x acc -> (x `S.isSubsetOf` circleMoves) || acc) False conditionSets
        crossWin = foldr (\x acc -> (x `S.isSubsetOf` crossMoves) || acc) False conditionSets

rvrtFunc :: Moves -> ExceptT String IO (Move, Moves)
rvrtFunc (Moves []) = throwError "Nothing to revert"
rvrtFunc (Moves (x:xs)) = pure (x, Moves xs)

revertOneStep :: StateT Moves (ExceptT String IO) Move
revertOneStep = StateT rvrtFunc

isDraw :: Moves -> Bool
isDraw (Moves moves) = length moves >= 9

move :: Player -> Position -> StateT Moves (ExceptT String IO) ()
move player pos = do
    Moves moves <- get
    let winner = whoWon moves
        takenPositions = getOccupied moves
        lastPlayer = getLastPlayer moves
    -- If board state given has a winner, then don't do anything
    case winner of
        -- If there are no winners, check if input is valid
        Nothing -> if (lastPlayer == player) || (pos `elem` takenPositions)
            -- Wrong input will cause the state to not change, and same player can move again
            then do
                liftIO $ putStrLn "Bad input"
                pure ()
            -- If everything is valid, add move to stack
            else
                let newState = Move (player, pos) : moves
                    winner = whoWon newState
                in case winner of
                    Nothing -> do
                        if isDraw $ Moves newState
                            then do
                                liftIO $ putStrLn "Draw!"
                                throwError "Draw!"
                            else put (Moves newState)
                    _ -> do
                        liftIO $ putStrLn (show player ++ " wins!")
                        throwError $ show player ++ " wins!"
        -- If there is a winner, give the original board back unchanged
        _ -> do
            liftIO $ putStrLn (show lastPlayer ++ " wins!")
            throwError (show lastPlayer ++ " wins!")

conditionSets :: [S.Set Position]
conditionSets = S.fromList <$> [
      [TopLeft, Top, TopRight]
    , [TopLeft, CenterLeft, BottomLeft]
    , [TopLeft, Center, BottomRight]
    , [Top, Center, Bottom]
    , [TopRight, CenterRight, BottomRight]
    , [TopRight, Center, BottomLeft]
    , [CenterLeft, Center, CenterRight]
    , [BottomLeft, Bottom, BottomRight]
    ]

-- Testing

testBoard :: StateT Moves (ExceptT String IO) ()
testBoard = do
    move Circle Center
    move Cross TopLeft
    move Circle TopRight
    move Cross BottomRight
    move Circle BottomLeft
    move Cross Bottom


tester :: Moves
tester = Moves [Move (Circle, TopRight), Move (Cross, TopLeft), Move (Circle, BottomRight)]

printBoard :: StateT Moves (ExceptT String IO) ()
printBoard = do
    liftIO $ putStrLn "board"
    board <- testBoard
    liftIO $ print board