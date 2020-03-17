module Game where

import Control.Monad
import Data.Array

import GameDef


-- Question 1.

-- pours from source jug to target jug given source jug, target jug
-- returns the updated (source jug, target jug)
pour :: Jug -> Jug -> (Jug, Jug)
pour (Jug capSrc curSrc) (Jug capTg curTg) =
    ((Jug capSrc newSrc),(Jug capTg newTg))
  where
    spaceLeftTg = capTg - curTg
    -- update the source jug and target jar after
    -- source jar poured into target jar
    newSrc
        | curSrc <= spaceLeftTg = 0
        | otherwise = curSrc - spaceLeftTg
    newTg
        | curSrc >= spaceLeftTg = capTg
        | otherwise = curTg + curSrc

-- checks if goal is reached given:
-- jugs, position of the jug in question, goal litres of jug
-- returns True if goal is reached, and False if not
finishedGoal :: Array Int Jug -> Int -> Integer -> Bool
finishedGoal jugs i x
    | getJugCur jugs i == x = True -- check if the ith jug has x litres
    | otherwise = False

-- update the jugs array after pouring from source jug to target jug
updateJugs :: Array Int Jug -> Jug -> Jug -> Int -> Int -> Array Int Jug
updateJugs jugs srcJug tgJug src tg = newJugs2
  where 
    -- get the new values of the source, target jugs
    (newSrcJug, newTgJug) = pour srcJug tgJug
    -- update the source jug
    newJugs1 = jugs // [(i, newSrcJug) | i <- [src]]
    -- update the target jug
    newJugs2 = newJugs1 // [(i, newTgJug) | i <- [tg]]

-- get the amount of water in ith jug
-- given the array of jugs, i
getJugCur :: Array Int Jug -> Int -> Integer 
getJugCur jugs i = cur
  where 
    (Jug cap cur) = jugs ! i

-- The game master for the jug game.  The parameters are the initial jug state
-- and the goal.
jugGame :: MonadGame m => Array Int Jug -> Goal -> m ()
jugGame jugs goal@(Goal i x) = do
    -- get number of jugs
    let (x1, x2) = bounds jugs
    -- get the source, target jug numbers specified by the player
    (FromTo src tg) <- gmAction jugs goal
    -- check if the jugs number is less than the number of jugs
    -- (since the first jug is jug0)
    if x1 <= src && x1 <= tg && src <= x2 && tg <= x2
        then do
            -- get the source, target jugs based on player's input
            let srcJug@(Jug capSrc curSrc) = jugs ! src
            let tgJug@(Jug capTg curTg) = jugs ! tg
            -- if the player made a valid move, execute the move
            if capSrc >= curSrc && capTg >= capTg && srcJug /= tgJug
                then
                    do
                    -- get the jugs at source and target after pouring
                    -- from source jug to target jug
                    let newJugs = updateJugs jugs srcJug tgJug src tg
                    -- if the goal was accomplished, then end the game
                    if finishedGoal newJugs i x == True
                        then do
                            return ()
                        -- if the goal wasn't accomplished, keep looping
                        else do
                            jugGame newJugs goal
            -- if the player made an invalid move, keep looping
            else do
                jugGame jugs goal
    -- if the player specified a jug that doesn't exist, keep looping
     else do
        jugGame jugs goal

-- Question 2.

instance Functor GameTrace where
    -- If you are confident with your Monad instance, you can just write
    fmap = liftM

instance Applicative GameTrace where
    -- If you are confident with your Monad instance, you can just write
    pure = return

    -- If you are confident with your Monad instance, you can just write
    (<*>) = ap

instance Monad GameTrace where
    return a = Pure a
    Pure a >>= f = f a
    Step jugs goal next >>= f = Step jugs goal (\msg -> (next msg) >>= f)

instance MonadGame GameTrace where
    gmAction jugs goal = Step jugs goal (\msg@(FromTo src tg) -> return (msg))

-- Question 3.

-- checks if the game ended
-- returns true if yes, false if not
isPure :: GameTrace () -> Bool
isPure (Pure ()) = True
isPure (Step _ _ _) = False

-- checks if the result after pouring from source jug to target jug is correct
isCorrect :: (PlayerMsg -> GameTrace ()) -> Int -> Int ->
             (Integer, Integer) -> Goal -> Bool
isCorrect next src tg (correctSrcCur, correctTgCur) goal =
    -- first, we need to check if the game ends.
    isCorrectType gameTrace src tg correctSrcCur correctTgCur goal
  where
    gameTrace = next (FromTo src tg) -- next state of the jugs after pouring
-- in the example, in all 6 steps, the game cannot finish
-- so if the game ends then it's incorrect   
isCorrectType gameTrace src tg correctSrcCur correctTgCur goal
    | isPure gameTrace == True = False
    -- second, check if the new jar states are correct
    | otherwise = isCorrectValue srcJug tgJug
                  correctSrcCur correctTgCur goal newGoal
    where
        (Step jugs newGoal newNext) = gameTrace -- the new states
        srcJug = jugs ! src                     -- the source jug
        tgJug = jugs ! tg                       -- the target jar
-- to know if the states are correct, check for the following:
-- 1.if the source jug's value is correct after pouring
-- from source jar to target jar
-- 2.if the target jug's value is correct after pouring
-- from source jar to target jar
-- 3.if the goal is still correct (the goal is not modified)
-- if all of the 3 points are true, then the states are correct. 
-- otherwise incorrect
isCorrectValue (Jug capSrc curSrc) (Jug capTg curTg)
               correctSrcCur correctTgCur goal newGoal
    | curSrc == correctSrcCur && curTg == correctTgCur && 
      goal == newGoal = True
    | otherwise = False

-- get the amount of water the source jug, target jug are supposed to have
-- after pouring from source jug into target jug
-- input is the array of jugs, the source jug number, the target jug number
correctCur :: Array Int Jug -> Int -> Int -> (Integer, Integer)
correctCur jugs src tg = (curSrc, curTg)
  where 
    (newJugSrc, newJugTg) = pour (jugs ! src) (jugs ! tg)
    (Jug capSrc curSrc) = newJugSrc                       
    (Jug capTg curTg) = newJugTg

-- initialize jugs, goal, gametrace given the raw data jugs, goal
initialize :: (Array Int Jug -> Goal -> GameTrace ()) -> 
              [Jug] -> Int -> Integer -> (Array Int Jug, Goal, GameTrace())
initialize f jugList i x = (jugs, goal, gameTrace)
  where
    jugs = mkJugArray jugList
    goal = Goal i x
    gameTrace = f jugs goal

testOneStep :: (Array Int Jug -> Goal -> GameTrace ()) -> Maybe ()
testOneStep f = do
    -- initialize testcase
    let (jugs, goal, gameTrace) = initialize f [(Jug 7 5), (Jug 4 2), (Jug 5 2)] 
                                             2 1
    -- get the correct values of each pair of (source jug, target jug)
    -- in all 6 combinations
    let correct01@(correctCur0a, correctCur1c) = correctCur jugs 0 1
    let correct02@(correctCur0b, correctCur2c) = correctCur jugs 0 2
    let correct10@(correctCur1a, correctCur0c) = correctCur jugs 1 0
    let correct12@(correctCur1b, correctCur2d) = correctCur jugs 1 2
    let correct20@(correctCur2a, correctCur0d) = correctCur jugs 2 0
    let correct21@(correctCur2b, correctCur1d) = correctCur jugs 2 1
    -- if the current state of jugs implies the end of the game
    -- then it's incorrect since we haven't accomplished our goal
    if isPure gameTrace == True
        then do
            return Nothing ()
        -- otherwise, the game hasn't ended so we should continue checking
        else do
            -- get the next states of all 6 different combinations of pouring
            let (Step jugs curGoal next) = gameTrace
            -- if the goal has not been modified and in all combinations,
            -- the pair of (source jug, target jug) are at correct values
            -- then it's correct
            if curGoal == goal 
                && isCorrect next 0 1 correct01 goal == True
                && isCorrect next 0 2 correct02 goal == True
                && isCorrect next 1 0 correct10 goal == True
                && isCorrect next 1 2 correct12 goal == True
                && isCorrect next 2 0 correct20 goal == True
                && isCorrect next 2 1 correct21 goal == True
                then do
                    return Just() ()
                -- otherwise it's incorrect
                else do
                    return Nothing ()

-- returns the result of whether the test pass or fail
returnResult :: Bool -> Maybe ()
returnResult True = return Just() ()
returnResult False = return Nothing ()

-- get the source, target jug numbers given move number
getSrcTg :: Int -> (Int, Int)
getSrcTg move
    | move == 1 = (2, 1)
    | move == 2 = (2, 0)

-- test for when the game isn't suppose to end
-- if game ended, test failed. if game doesn't end, test passed
testMove :: GameTrace () -> Int -> Bool
testMove (Step jug goal next) move = result
  where
    (src, tg) = getSrcTg move            -- the source, target jugs  
    newGameTrace = next (FromTo src tg)  -- the new game trace
    result
        -- if game ended, and we went through all the expected moves
        -- then test passed
        | isPure newGameTrace == True && move == 2 = True
        -- if game ended, and we didn't went through all the expected
        -- moves, then test failed
        | isPure newGameTrace == True && move < 2 = False
        -- if game doesn't end, then test for the next move
        -- if next move is not suppose to be the last move,
        -- then run the test for testing the next step
        | otherwise = testMove newGameTrace (move + 1)

-- check if the game finishes after making the right moves
testUntilDone :: (Array Int Jug -> Goal -> GameTrace ()) -> Maybe ()
testUntilDone f = returnResult result
  where
    (jugs, goal, gameTrace) = 
        initialize f [(Jug 2 0), (Jug 3 0), (Jug 4 4)] 0 1
    result
        -- if game ends, then test failed because in this example
        -- one move doesn't end the game
        | isPure gameTrace == True = False
        -- if game doesn't end, continue checking 
        | otherwise = testMove gameTrace 1  
