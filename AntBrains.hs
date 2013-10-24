module AntBrains where

import AntSkull
import Prelude hiding (drop, Right, Left)
import Control.Monad.State

-- Some function line numbers
_START        = 0
_TELL_FOOD    = 4
_TELL_FOEHOME = 3
_GET_FOOD     = 5

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
    -- We start by searching anything, food, enemies, whatever.               CURRENTLY FOOD ONLY
    start
    -- After we found anything, lets go tell the others what we found         CURRENTLY FOOD ONLY
    tell_others _FOEHOME
    tell_food

    -- Good, now we know the important things, so lets go pillage and raid. Arrrrrr!
    pillage_raid


-- The implementation functions for our strategy
start :: StateT Int IO ()
start = do
    lnr <- get
    senseAdj (lnr+1) (lnr+2) Food   -- 0: IF    there is food
    turnAround _TELL_FOOD         -- 1: THEN  turn around (because we want to run away) and start telling the others about the food
    randomMove lnr                -- 2: ELSE  do one step of a random walk and go on with what we do

tell_food :: StateT Int IO ()
tell_food = do
    lnr <- get
    nextL $ \n -> mark _FOOD n                       -- 3: tell others our mark
    nextL $ \n -> sense Ahead n (lnr+3) Home     -- 4: IF   the cell in front of me is my anthill
    nextL $ \n -> move n lnr                     -- 5: THEN move onto anthill
    drop _GET_FOOD                             -- 6:      drop food and return to searching
    randomMove lnr                             -- 7: ELSE do one step of the random walk and go on

tell_others :: Int -> StateT Int IO ()
tell_others m = do
    move 0 0

rally_troops :: StateT Int IO ()
rally_troops = do
    move 0 0

pillage_raid :: StateT Int IO ()
pillage_raid = do
    move 0 0

