{-# LANGUAGE RankNTypes #-}

module Tournament (
    evalM, handleAll,
    Bot(Bot), Choice(Cooperate, Defect), Moves,
    Player(Player), MatchResult,
    BotEnvironment, rand, time,
    payoff, totalScores, invert,
    runBot, runRound, runMatch, rounds,
    runRoundRobin, tabulateResults, eliminateHalf,
    showMatchPlayByPlay, displayTournament
    ) where

import Prelude
import Control.Applicative (Applicative)
import Control.Concurrent (threadDelay)
import Control.Exception (SomeException, handle)
import Control.Monad (replicateM, liftM2, liftM3)
import Data.List (nub, sort)
import Data.Map (fromListWith, toList)
import Data.Maybe (fromMaybe)
import Data.Tuple (swap)
import System.Random (randomRIO)
import System.Timeout (timeout)

_handle :: (SomeException -> IO (Maybe a)) -> IO (Maybe a) -> IO (Maybe a)
_handle = handle

evalM :: Monad m => m a -> m a
evalM = (>>= (return $!))

sequenceM2' :: Monad m => (m a, m b) -> m (a, b)
sequenceM2' = uncurry $ liftM2 (,)

sequenceM3' :: Monad m => (m a, m b, m c) -> m (a, b, c)
sequenceM3' = uncurry3 $ liftM3 (,,)
    where uncurry3 f (x, y, z) = f x y z

-------------- Basic setup for iterated PD --------------

data Choice = Cooperate | Defect
    deriving (Show, Read, Eq, Bounded, Enum)

-- The outcome of one round of PD is defined as the pair (Choice, Choice).
-- The outcomes of several rounds makes up a list of these pairs, which always
-- has the form:
-- [(your first move, their first move) ... (your last move, their last move)]
type Moves = (Choice, Choice)

payoff :: Moves -> (Int, Int)
payoff moves = case moves of
    (Defect, Defect)       -> (1, 1)
    (Defect, Cooperate)    -> (5, 0)
    (Cooperate, Defect)    -> (0, 5)
    (Cooperate, Cooperate) -> (3, 3)

-- Swap the positions of each players' moves in a list of previous moves.
-- E.g., [(your #1 move, their #1 move) ... (your #10 move, their #10 move)]
--    -> [(their #1 move, your #1 move) ... (their #10 move, your #10 move)]
invert :: [Moves] -> [Moves]
invert = map swap

totalScores :: [Moves] -> (Int, Int)
totalScores = (\(x,y)-> (sum x, sum y)) . unzip . map payoff

-------------- Bots, rounds, and matches --------------

-- A bot is a function that takes another bot and the history of the previous
-- rounds as input, and returns a Choice inside the BotEnvironment monad, which
-- is a subset of the IO monad that allows bots to generate random numbers and
-- simulate other bots with or without a timeout function. However, to prevent
-- bots from "breaking out of the box," any other forms of IO will not work
-- inside this monad.
newtype Bot = Bot { _runBot :: BotEnvironment m => Bot -> [Moves] -> m Choice }

runBot :: BotEnvironment m => Bot -> Bot -> [Moves] -> m Choice
runBot bot opp hist = do
    _pause 10
    _runBot bot opp hist

class (Functor m, Applicative m, Monad m) => BotEnvironment m where
    rand :: m Double
    time :: Int -> m a -> m (Maybe a)
    handleAll :: (SomeException -> m (Maybe a)) -> m (Maybe a) -> m (Maybe a)
    _pause :: Int -> m ()

-- The BotEnvironment monad provides two useful functions: "rand" which
-- generates a random float, and "time", which fully evaluates and runs and
-- action with a timeout
instance BotEnvironment IO where
    rand = randomRIO (0.0, 1.0)
    time i = timeout i . evalM
    handleAll = _handle
    _pause = threadDelay

-- A round pits two bots against each other, giving each access to the history
-- of all previous rounds and each other's source code. Each bot has 5 seconds
-- to make a move or else Defect will be automatically chosen.
runRound :: BotEnvironment m => Bot -> Bot -> [Moves] -> m Moves
runRound bot1 bot2 history = sequenceM2' (runBot1, runBot2)
  where runTimeout = fmap (fromMaybe Defect)
            . handleAll (\_ -> return Nothing) . time (5 * 10 ^ 6)
        runBot1 = runTimeout $ runBot bot1 bot2 history
        runBot2 = runTimeout $ runBot bot2 bot1 $ invert history

-- A match is simply a sequence of n rounds.
runMatch :: BotEnvironment m => Int -> Bot -> Bot -> m [Moves]
runMatch n b1 b2 = rounds n b1 b2 []

rounds :: BotEnvironment m => Int -> Bot -> Bot -> [Moves] -> m [Moves]
rounds n b1 b2 history
    | n <= 0       = return history
    | null history = sequence [runRound b1 b2 history] >>= rounds (n-1) b1 b2
    | otherwise    = sequence combinedHistory >>= rounds (n-1) b1 b2
  where combinedHistory = map return history ++ [runRound b1 b2 history]

-------------- Tournament structure --------------

-- A tournament is a series of round-robin elimination rounds. At the end of
-- each round-robin round, the bots with scores lower than the median score are
-- removed from the tournament pool. This process is repeated until only one
-- bot remains, or until there is a tie between all the remaining bots.

-- You can use these functions to run demo tournaments to battle-test your bot.
-- Check out the Bots.hs file for an example tournament.

data Player = Player { name :: String, bot :: Bot }

instance Show Player where
    show = name

instance Eq Player where
    bw1 == bw2 = name bw1 == name bw2

instance Ord Player where
    bw1 <= bw2 = name bw1 <= name bw2

type MatchResult = (Player, Player, [Moves])

showMatchPlayByPlay :: MatchResult -> String
showMatchPlayByPlay (b1, b2, hist) = name b1 ++ " vs " ++ name b2 ++ moves
  where moves = foldr ((++) . (++) "\n" . show) "" hist

-- A round-robin round consists of all combinations of bots playing one match
-- against each other.
runRoundRobin :: BotEnvironment m => Int -> [Player] -> m [MatchResult]
runRoundRobin n = mapM (sequenceM3' . result) . matches
  where result (b1:b2:_) = (return b1, return b2, runMatch n (bot b1) (bot b2))
        matches = filter (\x -> nub x == x) . nub . map sort . replicateM 2

-- A bot's combined score in a round-robin round is simply the sum of its score
-- from all matches it played.
tabulateResults :: [MatchResult] -> [(Player, Int)]
tabulateResults = sort . toList . fromListWith (+) . concatMap getPayoffs
  where getPayoffs (b1, b2, h) = [(b1, fst $ totalScores h),
                                  (b2, snd $ totalScores h)]

-- After a round-robin round, eliminate the lower-scoring half of the
-- tournament pool.
eliminateHalf :: [(Player, Int)] -> [Player]
eliminateHalf [] = []
eliminateHalf pairs = map fst $ filter ((>= median) . snd) pairs
  where median = med . sort $ map snd pairs
        med lst
            | odd $ length lst  = lst !! mid
            | even $ length lst = 1 + ((lst !! mid) + (lst !! (mid - 1))) `div` 2
        mid = length pairs `div` 2

-- Run a tournament and print it to stdout.
displayTournament :: Int -> [Player] -> IO ()
displayTournament n [] = putStrLn "No bots."
displayTournament n [winner] = putStr "Winner: " >> print winner
displayTournament n players = do
    putStrLn "\n<Start round-robin round>\n"
    robin <- runRoundRobin n players
    mapM_ (\x -> putStrLn (showMatchPlayByPlay x) >> putStrLn "") robin

    let scores = tabulateResults robin
    putStrLn "Scores after this round-robin round:"
    mapM_ print scores >> putStrLn ""

    if (length . nub $ map snd scores) == 1
        then putStrLn "Tie between these bots:" >> print players
        else let remainingBots = eliminateHalf scores in do
            putStrLn "Bots continuing to the next round:"
            print remainingBots >> putStrLn ""
            displayTournament n remainingBots
