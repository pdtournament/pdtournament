module Bots where

import Prelude
import Control.Monad (replicateM)

import Tournament

-------------- Some example bots --------------

-- Always cooperates
cooperateBot :: Bot
cooperateBot = Bot (\_ _ -> return Cooperate)

-- Always defects
defectBot :: Bot
defectBot = Bot (\_ _ -> return Defect)

-- 50% probability of cooperating, 50% probability of defecting
randomBot :: Bot
randomBot = Bot (\_ _ -> do
    choice <- rand
    return (if choice < 0.5 then Cooperate else Defect))

-- Do whatever my opponent did in the previous round; if it is the first round,
-- cooperate.
titForTatBot :: Bot
titForTatBot = Bot (\_ history -> case history of
            [] -> return Cooperate
            xs -> return . snd $ last xs)

-- Simulate my opponent playing one round against me, and do whatever my
-- opponent does. Note that when mirrorBot plays against itself, it will
-- defect, since it will simulate playing against itself, creating an infinite
-- chain of recursive function calls that will time out.
mirrorBot :: Bot
mirrorBot = Bot (\op hist -> runBot op mirrorBot $ invert hist)

-- Same as above, but only wait 1/100th of a second before terminating the
-- simulation, and cooperate if the simulation did not finish or raised an
-- exception.

smarterMirrorBot :: Bot
smarterMirrorBot = Bot (\op hist -> do
    simulation <- time 10000 . runBot op mirrorBot $ invert hist
    return (case simulation of
                Nothing   -> Cooperate
                Just move -> move))

-- Simulate my opponent playing the current round against cooperateBot 50
-- times, and cooperate if my opponent always cooperated; if it took more than
-- 1/100th of a second to move or defected, then defect.
justiceBot :: Bot
justiceBot = Bot (\op hist -> do
    sims <- replicateM 50 . time 10000 . runBot op cooperateBot $ invert hist
    return (if Just Defect `elem` sims || Nothing `elem` sims
                then Defect
                else Cooperate))

-------------- Example tournament --------------

examplePlayers :: [Player]
examplePlayers = [ Player "CooperateBot" cooperateBot
                 , Player "DefectBot" defectBot
                 , Player "RandomBot" randomBot
                 , Player "TitForTatBot" titForTatBot
                 , Player "SmarterMirrorBot" smarterMirrorBot
                 , Player "JusticeBot" justiceBot ]

-- A sample tournament with 10 rounds per match.
exampleTournament :: [Player] -> IO ()
exampleTournament contestants = do
    results <- runTournament 10 contestants
    mapM_ (\x -> putStrLn (showMatchPlayByPlay x) >> putStrLn "") results
    putStrLn "Final scores:"
    mapM_ print $ tabulateResults results

-- Run the sample tournament.
runExample :: IO ()
runExample = exampleTournament examplePlayers
