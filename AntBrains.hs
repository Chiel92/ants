module AntBrains where

import AntSkull
import Prelude hiding (drop, Right, Left)
import Control.Monad.State

-- Some function line numbers
_START        = 0          -- First
_TELL_FOEHOME = 18         -- #Start
_TELL_FOOD    = 18+1       -- #Start + #TellFoeHome
_GET_FOOD     = 0          -- First
_PILLAGE_RAID = 18+1+13    -- #Start + #TellFoeHome + #TellFood

-- The markers
_FOEHOME = 0
_FOOD    = 1


-- A main function to run the stuff
main :: IO ()
main = debug program

-- And one to debug strategy programs
debug :: StateT Int IO () -> IO ()
debug prog = do
    runStateT prog 0
    return ()

-- Our strategy
program :: StateT Int IO ()
program = do
    -- We start by searching anything, food, enemies, whatever.               CURRENTLY FOOD ONLY
    start
    -- After we found anything, lets go tell the others what we found
    tell_foehome
    tell_food

    -- Good, now we know the important things, so lets go pillage and raid. Arrrrrr!
    pillage_raid


-- The implementation functions for our strategy
start :: StateT Int IO ()
start = do                                                    -- Total: 18 = 13+1 + 5-1
    lnr <- get
    senseAdjMoveAndNot (lnr+9) (lnr+13) (lnr+13) Food Home    -- 0:  IF    there is food (not on Home) and we moved to it
    nextL $ \n -> pickup n (lnr+13)                           -- 9:  THEN  pickup the food
    turnAround _TELL_FOOD                                     -- 10:       turn around (because we want to run away) and start telling the others about the food
    randomMove lnr                                            -- 13: ELSE  do one step of a random walk and go on with what we do

tell_foehome :: StateT Int IO ()
tell_foehome = do                   -- Total: 1 = 0+1 + 1-1
    move 0 0                        -- 0: Do nothing useful inparticular

tell_food :: StateT Int IO ()
tell_food = do                                             -- Total: 13 = 8+1 + 5-1
    lnr <- get
    nextL $ \n -> mark _FOOD n                             -- 0: tell others our mark
    senseAdjMove (lnr+7) (lnr+8) (lnr+8) Home              -- 1: IF   an adjacent cell is my anthill and I moved there
    drop _GET_FOOD                                         -- 7: THEN drop the food and return to searching
    randomMove lnr                                         -- 8: ELSE do one step of the random walk and go on

pillage_raid :: StateT Int IO ()
pillage_raid = do                             -- Total: 1
    move 0 0

rally_troops :: StateT Int IO ()
rally_troops = do
    move 0 0

run :: StateT Int IO ()
run = do
    move 0 0

