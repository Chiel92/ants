module AntBrains where

import AntSkull
import Prelude hiding (drop, Right, Left)
import Control.Monad.State

-- Some function line numbers
_START   = 0
_GO_HOME = 9

-- Some markers
_FOE  = 0
_FOOD = 1


test_main :: IO ()
test_main =
  do
    runStateT test_program 0
    return ()

test_program :: StateT Int IO ()
test_program =
  do
    random (0.2) 0 0
    comment "BOO"
    random (0.5) 0 0
    comment "BOO"
    random (0.4) 0 0



main :: IO ()
main = do
    -- We start by searching anything, food, enemies, whatever.               CURRENTLY FOES ONLY
    start
    -- After we found anything, lets go tell the others what we found         CURRENTLY FOES ONLY
    tell_others _FOE
    tell_others _FOOD -- (The other function - line numbers incorrect)

    -- Good, now we know the important things, so lets go pillage and raid. Arrrrrr!
    pillage_raid

start :: IO ()
start = do
    f_nr <- get
    senseAdj 0 1 3 Foe      -- 0: IF ENEMIES
    mark _FOE 2             -- 1: THEN  Inform the others
    move 2 0                -- 2:       move onto food (return to state 0 on failure)
    pickup _GO_HOME 0       -- 3:       pick up food and jump to state 8 (or 0 on failure)
    rand 3 4 5              -- 4: ELSE  choose whether to...
    turn Left 0             -- 5:       turn left and return to state 0
    rand 2 6 7              -- 6:       ...or...
    turn Right 0            -- 7:       turn right and return to state 0
    move 0 3                -- 8:       ...or move forward and return to state 0 (or 3 on failure)

tell_others :: Mark -> IO()
tell_others m = do
    -- n <- get     -- n = function line nr
    sense Ahead 9 11 Home   -- 9: IF the cell in front of me is my anthill
    move 10 8               -- 10: THEN   move onto anthill
    drop _START             -- 11:      drop food and return to searching
    rand 3 12 13            -- 12: ELSE choose whether to...
    turn Left 8             -- 13:      turn left and return to state 8
    rand 2 14 15            -- 14:      ...or...
    turn Right 8            -- 15:      turn right and return to state 8
    move 8 11               -- 16:      ...or move forward and return to state 8

pillage_raid :: IO ()
pillage_raid = do
    move 0 0

