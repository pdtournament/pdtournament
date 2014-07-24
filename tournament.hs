module IPD where

import Control.Monad (replicateM, liftM2, liftM3)
import Data.List (nub, sort)
import Data.Map (fromListWith, toList)
import Data.Tuple (swap)

import Control.Monad.Random (Rand, StdGen, getRandomR, evalRandIO)

sequenceM2' :: Monad m => (m a, m b) -> m (a, b)
sequenceM2' = uncurry $ liftM2 (,)

sequenceM3' :: Monad m => (m a, m b, m c) -> m (a, b, c)
sequenceM3' = uncurry3 $ liftM3 (,,)
    where uncurry3 f (x, y, z) = f x y z

-------------- Basic setup for iterated PD --------------

data Choice = Cooperate | Defect
    deriving (Show, Read, Eq, Bounded, Enum)

-- the outcome of one round of PD is defined as the pair (Choice, Choice)
-- the outcomes of several rounds makes up a list of these pairs, which always
-- has the form:
-- [(your first move, their first move) ... (your last move, their last move)]
type Moves = (Choice, Choice)

payoff :: Moves -> (Int, Int)
payoff moves = case moves of
    (Defect, Defect)       -> (1, 1)
    (Defect, Cooperate)    -> (5, 0)
    (Cooperate, Defect)    -> (0, 5)
    (Cooperate, Cooperate) -> (3, 3)

totalScore :: [Moves] -> (Int, Int)
totalScore = (\(x, y) -> (sum x, sum y)) . unzip . map payoff

-------------- Bots, rounds, and matches --------------

-- a bot is a function that takes another bot and the history of the previous
-- rounds as input, and returns a Choice inside the Rand monad (so that the bot
-- can use random numbers if it desires)
newtype Bot = Bot { runBot :: Bot -> [Moves] -> Rand StdGen Choice }

-- a round pits two bots against each other, giving each access to the history
-- of all previous rounds and each other's source code
runRound :: Bot -> Bot -> [Moves] -> Rand StdGen Moves
runRound bot1 bot2 history = sequenceM2' (runBot1, runBot2)
  where runBot1 = runBot bot1 bot2 history
        runBot2 = runBot bot2 bot1 $ map swap history

-- a match is simply a sequence of n rounds
runMatch :: Int -> Bot -> Bot -> Rand StdGen [Moves]
runMatch n bot1 bot2 = rounds n bot1 bot2 []

rounds :: Int -> Bot -> Bot -> [Moves] -> Rand StdGen [Moves]
rounds n b1 b2 history
    | n <= 0       = return history
    | null history = sequence [runRound b1 b2 history] >>= rounds (n-1) b1 b2
    | otherwise    = sequence combinedHistory >>= rounds (n-1) b1 b2
  where combinedHistory = map return history ++ [runRound b1 b2 history]

-------------- Tournament structure --------------

data BotWrapper = BotWrapper { name :: String, bot :: Bot }

instance Show BotWrapper where
    show = name

instance Eq BotWrapper where
    bw1 == bw2 = name bw1 == name bw2

instance Ord BotWrapper where
    bw1 <= bw2 = name bw1 <= name bw2

type MatchResult = (BotWrapper, BotWrapper, [Moves])

-- a tournament consists of all combinations of bots playing one match against
-- each other (including each bot playing one match against itself)
runTournament :: Int -> [BotWrapper] -> Rand StdGen [MatchResult]
runTournament n = mapM (sequenceM3' . round) . matches
  where round (b1:b2:_) = (return b1, return b2, runMatch n (bot b1) (bot b2))
        matches = nub . map sort . replicateM 2

tabulateResults :: [MatchResult] -> [(BotWrapper, Int)]
tabulateResults = sort . toList . fromListWith (+) . concatMap getPayoffs
  where getPayoffs (b1, b2, h) = [(b1, fst $ totalScore h),
                                  (b2, snd $ totalScore h)]

showMatchPlayByPlay :: MatchResult -> String
showMatchPlayByPlay (b1, b2, hist) = name b1 ++ " , " ++ name b2 ++ moves
  where moves = foldr ((++) . (++) "\n" . show) hist

-------------- Some example bots --------------

-- always cooperates
cooperateBot :: Bot
cooperateBot = Bot (\_ _ -> return Cooperate)

-- always defects
defectBot :: Bot
defectBot = Bot (\_ _ -> return Defect)

-- 50% probability of cooperating, 50% probability of defecting
randomBot :: Bot
randomBot = Bot (\_ _ -> do
    choice <- getRandomR (True, False)
    return (if choice
                then Cooperate
                else Defect))

-- do whatever my opponent did in the last round
titForTatBot :: Bot
titForTatBot = Bot (\_ history -> case history of
            [] -> return Cooperate
            xs -> return . snd $ last history)

-- simulate my opponent playing the current round against cooperateBot 20
-- times, and cooperate if my opponent always cooperates; otherwise defect
justiceBot :: Bot
justiceBot = Bot (\op hist -> do
    simulatedRounds <- replicateM 20 . runBot op cooperateBot $ map swap hist
    return (if Defect `elem` simulatedRounds
                then Defect
                else Cooperate))

-- simulate my opponent playing 5 full matches against cooperateBot, and
-- cooperate if my opponent always cooperates; otherwise defect
extremeJusticeBot :: Bot
extremeJusticeBot = Bot (\op hist -> do
    simulatedMatches <- replicateM 5 $ runMatch 10 op cooperateBot
    return (if Defect `elem` map fst $ concat simulatedMatches
                then Defect
                else Cooperate))

-------------- Example tournament --------------

exampleBots :: [BotWrapper]
exampleBots = [ BotWrapper "CooperateBot" cooperateBot
              , BotWrapper "DefectBot" defectBot
              , BotWrapper "RandomBot" randomBot
              , BotWrapper "TitForTatBot" titForTatBot
              , BotWrapper "JusticeBot" justiceBot
              , BotWrapper "XJusticeBot" extremeJusticeBot ]

runExample :: [BotWrapper] -> IO ()
runExample wrappedbots = do
    results <- evalRandIO $ runTournament 10 wrappedbots
    mapM_ (\x -> putStrLn (showMatchPlayByPlay x) >> putStrLn "") results
    mapM_ print $ tabulateResults results
