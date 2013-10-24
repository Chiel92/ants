module AntBrains where

import AntSkull
import Prelude hiding (drop, Right, Left)
import Control.Monad.State

-- Some function line numbers
_START        = 0
_TELL_FOOD    = 18
_TELL_FOEHOME = 0

-- The markers
_FOEHOME = 0
_FOOD    = 1


-- A main function to run the stuff
main :: IO ()
main = do
    runStateT program 0
    return ()

-- Our strategy
program :: StateT Int IO ()
program = do
    randomMove 1
    comment "END TEST"
    -- We start by searching anything, food, enemies, whatever.               CURRENTLY FOOD ONLY
    start
    -- After we found anything, lets go tell the others what we found         CURRENTLY FOOD ONLY
    tell_others _FOEHOME
    tell_others _FOOD -- (The other function - line numbers incorrect)

    -- Good, now we know the important things, so lets go pillage and raid. Arrrrrr!
    pillage_raid


-- The implementation functions for our strategy
start :: StateT Int IO ()
start = do
    lnr <- get
    curL $ \l -> senseAdj 0 1 (l+2) Food   -- 0: IF there is food
    turnAround _TELL_FOOD                -- 1: THEN  turn around (because we want to run away) and start telling the others about the foehome
    rand 3 4 5                           -- 2: ELSE  choose whether to...
    turn Left 0                          -- 3:       turn left and return to state 0
    rand 2 6 7                           -- 4:       ...or...
    turn Right 0                         -- 5:       turn right and return to state 0
    move 0 3                             -- 6:       ...or move forward and return to state 0 (or 3 on failure)

tell_others :: Mark -> StateT Int IO ()
tell_others m = do
    lnr <- get
    sense Ahead (lnr + 1) 11 Home -- 9: IF the cell in front of me is my anthill
    move 10 8                     -- 10: THEN   move onto anthill
    drop _START                   -- 11:      drop food and return to searching
    rand 3 12 13                  -- 12: ELSE choose whether to...
    turn Left 8                   -- 13:      turn left and return to state 8
    rand 2 14 15                  -- 14:      ...or...
    turn Right 8                  -- 15:      turn right and return to state 8
    move 8 11                     -- 16:      ...or move forward and return to state 8

rally_troops :: StateT Int IO ()
rally_troops = do
    move 0 0

pillage_raid :: StateT Int IO ()
pillage_raid = do
    move 0 0

