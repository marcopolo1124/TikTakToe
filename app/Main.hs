module Main where
import qualified Data.Map as Map
import qualified Data.Set as S
import Control.Monad.State
import Control.Monad.Except
import Lib
import Data.Char


main :: IO (Either String ((), Moves))
main = do
  putStrLn instructions
  runExceptT $ runStateT gameLoop (Moves [])

positionStrings :: Map.Map [Char] Position
positionStrings = Map.fromList [("TL", TopLeft), ("T", Top), ("TR", TopRight), ("CL", CenterLeft), ("C", Center), ("CR", CenterRight), ("BL", BottomLeft), ("B", Bottom), ("BR", BottomRight)]


instructions :: String
instructions = "Enter TL, T, TR, CL, C, CR, BL, B, BR to control. Rows are split into T, C, B for top, center and bottom, whilst the columns are split into L, // , R for left, (neutral), and right" 

gameLoop :: StateT Moves (ExceptT String IO) ()
gameLoop = do
  board <- get
  liftIO $ print board
  -- fetch the player who last played to change players
  let lastPlayer = getLastPlayer $ getMoves board
      -- function to play move
      playMove player = do
        liftIO $ putStrLn (show player ++ " to move.")
        whereToMoveTo <- liftIO getLine
        if (toLower <$> whereToMoveTo) == "take back" 
          then do 
            revertOneStep 
            gameLoop
          else do
            let 
              maybePosition = Map.lookup whereToMoveTo positionStrings
            case maybePosition of
              Nothing -> do
                -- Repeat asking until the correct input is put in
                liftIO $ putStrLn $ "Bad input. " ++ instructions
                playMove player
              Just position -> do
                move player position
                gameLoop

  if lastPlayer == Circle
    then playMove Cross
    else playMove Circle


    

