import Control.Monad.Trans.State.Lazy
import Control.Monad
import Control.Monad.IO.Class
import System.IO
import System.Random
import Data.Bifunctor
import Data.Tuple
import qualified Data.Map.Lazy as Map

--           computer, player
type Score = (Int, Int)

type MoveKeys = (Int, Int)
type MoveValue = Int
type PlayerMoves = Map.Map MoveKeys MoveValue
type PriorTwoMoves = (Maybe Int, Maybe Int)

emptyGame = ((0,0), Map.empty, (Nothing, Nothing))

printMap :: PlayerMoves -> IO ()
printMap playerMoves = let
  llIO = Map.foldlWithKey (\ll key value -> (putStrLn ("key: "++(show key)++" value: "++(show value))):ll) [] playerMoves
  in sequence_ llIO

-- computer wins evens
-- player wins odds

main' :: IO ()
main' = (randomRIO (-1, 1) :: IO Int) >>= (\i -> putStrLn ("hello"++(show i)))

type Morra = StateT (Score, PlayerMoves, PriorTwoMoves) IO

readHand :: IO Int
readHand = do
  raw <- getLine
  let match x
        | x == "1" = 1
        | x == "2" = 2
        | x == "3" = 3
        | x == "4" = 4
        | x == "5" = 5
        | otherwise = 0
      fingers = match raw
  return fingers

-- Excercise to reader: change roundWinner to be `Int -> Morra String`
-- roundWinner :: Score -> Int -> (String, Score)
-- roundWinner priorScore fingers
--   | mod fingers 2 == 0 = ("Computer Wins Round; finger sum "++(show fingers), first (+1) priorScore)
--   | otherwise = ("Player Wins Round; finger sum "++(show fingers), second (+1) priorScore)

roundWinner :: Int -> Morra String
roundWinner fingers
  | mod fingers 2 == 0 = do
      (priorScore, playerMoves, priorTwoMoves) <- get
      _ <- put $ (first (+1) priorScore, playerMoves, priorTwoMoves)
      return ("Computer Wins Round; finger sum "++(show fingers))
  | otherwise = do
      (priorScore, playerMoves, priorTwoMoves) <- get
      _ <- put $ (second (+1) priorScore, playerMoves, priorTwoMoves)
      return ("Player Wins Round; finger sum "++(show fingers))

-- roundWinner ps f =  swap . (roundWinner' ps f)
  
winner :: Score -> String
winner (comp, player)
  | comp == player = "Tie"
  | comp > player = "Computer Wins"
  | otherwise = "Player Wins"

-- prediction of player move, based on last two moves, if they exist
prediction :: Morra (Maybe Int)
prediction = do
  (_, playerMoves, priorTwoMoves) <- get  
  case priorTwoMoves of
    ((Just secondLastMove), (Just lastMove)) -> pure (Map.lookup (secondLastMove, lastMove) playerMoves)
    (_, _) -> pure Nothing

-- computer makes decision on number of fingers
computerMove :: Morra Int
computerMove = do
  (_, playerMoves, priorTwoMoves) <- get
  maybePrediction <- prediction
  case maybePrediction of
    Nothing -> liftIO (randomRIO (0, 5))
    (Just predictedPlayerMove) -> pure $ if (mod predictedPlayerMove 2 == 0) then 0 else 1

storePlayerMoves :: Int -> Morra ()
storePlayerMoves currentPlayerMove = do
  (priorScore, playerMoves, priorTwoMoves) <- get
  let playerMoves' = case priorTwoMoves of
        ((Just secondLastMove), (Just lastMove)) -> Map.insert (secondLastMove, lastMove) currentPlayerMove playerMoves
        (_, _) -> playerMoves
  let priorTwoMoves' = case priorTwoMoves of
        ((_), maybeLastMove) -> (maybeLastMove, Just currentPlayerMove)
        (_, _) -> priorTwoMoves
  _ <- put (priorScore, playerMoves', priorTwoMoves')
  return ()
    

-- :t liftIO
-- liftIO :: MonadIO m => IO a -> m a

-- This is a Morra computation (type Morra String = StateT Score IO String).
-- Within this computation, we're able to perform IO actions as well as manipulating a state
-- of type Score. The result of the computation has type `String`.
morraRound :: Morra String
morraRound = do -- StateT Score IO String
  -- Get the current/previous score
  (_, playerMoves, (secondLastMove, lastMove)) <- get

  computerFingerCount <- computerMove
  -- Random fingers
  --computerFingerCount <- liftIO $ randomRIO (0, 5)
  playerFingerCount <- liftIO readHand

  _ <- storePlayerMoves playerFingerCount
  
  -- Determine the round winner
  message <- roundWinner (computerFingerCount + playerFingerCount)
 
  -- Store the new score
  -- put newscore
  
  -- Describe what happen during the round, this message is the result of the computation.
  -- The `a` value of (StateT Score IO a).
  return message

tenMorraRounds :: Morra [String]
tenMorraRounds = replicateM 10 morraRound

-- Run the game by executing all these rounds; printing the messages and also our final winner.
-- The point is that
-- (1) morraRound is a single round that has IO effects as well as being stateful with messages as a result.
-- (2) tenMorraRounds is ten of those rounds, collecting the results (the state and effects are carried out properly)
-- (3) Eventually we run that tenMorraRounds computation and we obtain the messages results, as well as the final state (so the Score).
-- coderpad.io is limited. Almost no libraries at all :(
-- I will copy it into Emacs and run it.  Thank you both very much!!
-- Repaste here if anything.  Will do

-- https://coderpad.io/MAF264KT
game :: IO ()
game = do
  (collectedMessages, (finalScore, playerMoves, priorTwoMoves)) <- runStateT tenMorraRounds emptyGame
  _ <- mapM_ putStrLn collectedMessages
  _ <- putStrLn (winner finalScore ++ "  " ++ show finalScore)
  -- putStrLn $ show playerMoves
  _ <- putStrLn ("map size " ++ (show (Map.size playerMoves)))
  printMap playerMoves
