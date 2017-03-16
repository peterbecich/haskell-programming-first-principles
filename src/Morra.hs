import Control.Monad.Trans.State.Lazy
import Control.Monad
import System.IO
import System.Random
import Data.Bifunctor
import Data.Tuple

--           computer, player
type Score = (Int, Int)

-- computer wins evens
-- player wins odds

main' :: IO ()
main' = (randomRIO (-1, 1) :: IO Int) >>= (\i -> putStrLn ("hello"++(show i)))

type Morra = StateT Score IO

readHand :: IO Int
readHand = do
  raw <- getChar
  let match x
        | x == '1' = 1
        | x == '2' = 2
        | x == '3' = 3
        | x == '4' = 4
        | x == '5' = 5
        | otherwise = 0
      fingers = match raw
  return fingers


roundWinner :: Score -> Int -> (String, Score)
roundWinner priorScore fingers
  | mod fingers 2 == 0 = ("Computer Wins Round; finger sum "++(show fingers), first (+1) priorScore)
  | otherwise = ("Player Wins Round; finger sum "++(show fingers), second (+1) priorScore)

-- roundWinner ps f =  swap . (roundWinner' ps f)
  
winner :: Score -> String
winner (comp, player)
  | comp == player = "Tie"
  | comp > player = "Computer Wins"
  | otherwise = "Player Wins"

morraRound :: Morra String
morraRound = do
  score <- get
  let computerFingerCount = randomRIO (0, 5) :: IO Int
      playerFingerCount = readHand :: IO Int
      fingerCount = liftM2 (+) computerFingerCount playerFingerCount :: IO Int
      out = liftM2 roundWinner (pure score) fingerCount :: IO (String, Score)
  StateT (\_ -> out)

tenMorraRounds :: Morra Score
tenMorraRounds = replicateM 10 morraRound >> get

game :: IO String
game = evalStateT (liftM winner tenMorraRounds) (0, 0)
