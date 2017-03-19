{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE Rank2Types #-}

module Morra where

import Control.Monad.Trans.State.Lazy
import Control.Monad
import Control.Monad.IO.Class
import Control.Lens
import Control.Monad.Trans.Class
import Control.Monad.Trans.State
import System.IO
import System.Random
import Data.Bifunctor
import Data.Tuple
import qualified Data.Map.Lazy as Map


-- computer wins evens
-- player wins odds

--           computer, player
data Score = Score
  { _computer :: Int,
    _player :: Int
  } deriving (Show)

type Fingers = Int
type PlayerMoveKeys = (Int, Int)
type PlayerMoves = Map.Map PlayerMoveKeys Fingers
data PriorTwoMoves = PriorTwoMoves
  { _secondLastMove :: Maybe Fingers,
    _lastMove :: Maybe Fingers
  } deriving (Show)

tupleMoves :: PriorTwoMoves -> (Maybe Fingers, Maybe Fingers)
tupleMoves (PriorTwoMoves one two) = (one, two)

flipTuple :: (Maybe a, Maybe b) -> Maybe (a, b)
flipTuple ((Just x), (Just y)) = Just (x, y)
flipTuple _ = Nothing

data GameState = GameState
  { _score :: Score,
    _playerMoves :: PlayerMoves,
    _priorTwoMoves :: PriorTwoMoves
  } deriving (Show)

makeLenses ''Score
makeLenses ''PriorTwoMoves
makeLenses ''GameState

emptyScore = Score 0 0
emptyPlayerMoves = Map.empty
emptyPriorTwoMoves = PriorTwoMoves Nothing Nothing
emptyGame = GameState emptyScore emptyPlayerMoves emptyPriorTwoMoves

printMap :: PlayerMoves -> IO ()
printMap playerMoves = let
  llIO = Map.foldlWithKey (\ll key value -> (putStrLn ("key: "++(show key)++" value: "++(show value))):ll) [] playerMoves
  in sequence_ llIO

type Morra = StateT GameState IO

readHand :: IO Fingers
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

roundWinner :: Fingers -> Morra String
roundWinner fingers
  | mod fingers 2 == 0 = do
      --     arrow? V
      --_ <- put $ (first (+1) priorScore, playerMoves, priorTwoMoves)
      _ <- score.computer += 1
      return ("Computer Wins Round; finger sum "++(show fingers))
  | otherwise = do
      _ <- score.player += 1
      return ("Player Wins Round; finger sum "++(show fingers))

winner :: Score -> String
winner (Score comp player)
  | comp == player = "Tie"
  | comp > player = "Computer Wins"
  | otherwise = "Player Wins"

-- prediction of player move, based on last two moves, if they exist
prediction :: Morra (Maybe Fingers)
prediction = do
  gameState <- get
  case gameState of
    (GameState _ playerMoves priorTwoMoves) ->
      case priorTwoMoves of
        (PriorTwoMoves (Just secondLastMove) (Just lastMove)) -> pure (Map.lookup (secondLastMove, lastMove) playerMoves)
        (PriorTwoMoves _ _) -> pure Nothing

-- computer makes decision on number of fingers
computerMove :: Morra Fingers
computerMove = do
  gameState <- get
  maybePrediction <- prediction
  case gameState of
    (GameState _ playerMoves priorTwoMoves) ->
      case maybePrediction of
        Nothing -> liftIO (randomRIO (0, 5))
        (Just predictedPlayerMove) -> pure $ if (mod predictedPlayerMove 2 == 0) then 0 else 1

storePlayerMoves :: Fingers -> Morra ()
storePlayerMoves currentPlayerMove = do
  gameState <- get
  let
    lm = gameState^.priorTwoMoves.lastMove
  _ <- priorTwoMoves.secondLastMove .= lm
  _ <- priorTwoMoves.lastMove .= (Just currentPlayerMove)
  return ()

cachePlayerMove' :: Fingers -> (Fingers, Fingers) -> Morra ()
cachePlayerMove' currentFinger priorTwo = do
  gameState <- get
  let playerMoves' = gameState^.playerMoves
  playerMoves .= Map.insert priorTwo currentFinger playerMoves'

cachePlayerMove :: Fingers -> Morra ()
cachePlayerMove fingers = do
  gameState <- get
  let
    lastTwoMaybe = tupleMoves $ gameState^.priorTwoMoves  :: (Maybe Fingers, Maybe Fingers)
    maybeLastTwo = flipTuple lastTwoMaybe :: Maybe (Fingers, Fingers)
  mapM_ (cachePlayerMove' fingers) maybeLastTwo


-- This is a Morra computation (type Morra String = StateT Score IO String).
-- Within this computation, we're able to perform IO actions as well as manipulating a state
-- of type Score. The result of the computation has type `String`.
morraRound :: Morra String
morraRound = do -- StateT Score IO String
  -- Get the current/previous score
  -- gameState <- get

  computerFingerCount <- computerMove
  playerFingerCount <- liftIO readHand

  _ <- storePlayerMoves playerFingerCount
  _ <- cachePlayerMove playerFingerCount

  -- Determine the round winner
  message <- roundWinner (computerFingerCount + playerFingerCount)
  
  -- Describe what happened during the round, this message is the result of the computation.
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
  messagesGameState <- runStateT tenMorraRounds emptyGame
  _ <- mapM_ putStrLn $ fst messagesGameState
  let gameState = snd messagesGameState
  -- let finalScore = (snd gameState).score
  -- _ <- putStrLn (winner finalScore ++ "  " ++ show finalScore)
  -- -- putStrLn $ show playerMoves
  -- _ <- putStrLn ("map size " ++ (show (Map.size playerMoves)))
  -- printMap gameState.playerMoves
  _ <- putStrLn $ show $ _score gameState
  _ <- putStrLn $ show $ _priorTwoMoves gameState
  printMap (_playerMoves gameState)
