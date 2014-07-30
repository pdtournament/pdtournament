{-# LANGUAGE RankNTypes #-}

module Tournament (
    handleAll, evalM,
    Bot(Bot), Choice(Cooperate, Defect), Moves,
    Player(Player), MatchResult,
    BotEnvironment, rand, time,
    payoff, totalScores, invert,
    runBot, runRound, runMatch, rounds,
    runTournament, tabulateResults, showMatchPlayByPlay,
    ) where

import Prelude
import Control.Applicative (Applicative)
import Control.Exception (SomeException, handle)
import Control.Monad (replicateM, liftM2, liftM3)
import Data.List (nub, sort)
import Data.Map (fromListWith, toList)
import Data.Maybe (fromMaybe)
import Data.Tuple (swap)
import System.Random (randomRIO)
import System.Timeout (timeout)

handleAll :: (SomeException -> IO (Maybe a)) -> IO (Maybe a) -> IO (Maybe a)
handleAll = handle

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
newtype Bot = Bot { runBot :: BotEnvironment m => Bot -> [Moves] -> m Choice }

class (Functor m, Applicative m, Monad m) => BotEnvironment m where
    rand :: m Double
    time :: Int -> m a -> m (Maybe a)

-- The BotEnvironment monad provides two useful functions: "rand" which
-- generates a random float, and "time", which fully evaluates and runs and
-- action with a timeout
instance BotEnvironment IO where
    rand = randomRIO (0.0, 1.0)
    time i = handleAll (\_ -> return Nothing) . timeout i . evalM

-- A round pits two bots against each other, giving each access to the history
-- of all previous rounds and each other's source code. Each bot has 5 seconds
-- to make a move or else Defect will be automatically chosen.
runRound :: BotEnvironment m => Bot -> Bot -> [Moves] -> m Moves
runRound bot1 bot2 history = sequenceM2' (runBot1, runBot2)
  where runTimeout = fmap (fromMaybe Defect) . time (5 * 10 ^ 6)
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

data Player = Player { name :: String, bot :: Bot }

instance Show Player where
    show = name

instance Eq Player where
    bw1 == bw2 = name bw1 == name bw2

instance Ord Player where
    bw1 <= bw2 = name bw1 <= name bw2

type MatchResult = (Player, Player, [Moves])

-- A tournament consists of all combinations of bots playing one match against
-- each other (including each bot playing one match against itself).
runTournament :: BotEnvironment m => Int -> [Player] -> m [MatchResult]
runTournament n = mapM (sequenceM3' . result) . matches
  where result (b1:b2:_) = (return b1, return b2, runMatch n (bot b1) (bot b2))
        matches = nub . map sort . replicateM 2

-- A bot's final score is simply their combined score from all matches played.
tabulateResults :: [MatchResult] -> [(Player, Int)]
tabulateResults = sort . toList . fromListWith (+) . concatMap getPayoffs
  where getPayoffs (b1, b2, h) = [(b1, fst $ totalScores h),
                                  (b2, snd $ totalScores h)]

showMatchPlayByPlay :: MatchResult -> String
showMatchPlayByPlay (b1, b2, hist) = name b1 ++ " vs " ++ name b2 ++ moves
  where moves = foldr ((++) . (++) "\n" . show) "" hist
