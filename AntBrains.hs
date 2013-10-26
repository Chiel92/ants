module AntBrains where

import AntSkull
import Prelude hiding (drop, Right, Left, (&&), (||))


-- The markers
_FOEHOME = 0
_FOOD    = 1
_HOME    = 5


-- Our strategy
program :: Entry -> M ()
program _Search = do
    _TellFoehome <- alloc
    _TellFood    <- alloc
    _GetFood     <- alloc
    _ReturnFood  <- alloc

    -- We start by searching anything, food, enemies, whatever.               CURRENTLY FOOD ONLY
    search _Search _TellFood _TellFoehome
    -- After we found anything, lets go tell the others what we found
    tell_foehome _TellFoehome
    tell_food _TellFood
    -- Now either continue to set a trap, or to get food
    get_food _GetFood
    return_food _ReturnFood


-- The implementation functions for our strategy
search :: Entry -> Entry -> Entry -> M ()
search _this _TellFood _TellFoehome = do
    _pickUp     <- alloc
    _turnAround <- alloc
    _randomWalk <- alloc

    senseAdjMoveAndNot _this _pickUp _randomWalk _randomWalk Food Home
    pickup _pickUp _turnAround _randomWalk
    turnAround _TELL_FOOD
    randomMove _randomWalk 

tell_foehome :: M ()
tell_foehome = do                   -- Total: 1 = 0+1 + 1-1
    move 0 0                        -- 0: Do nothing useful in particular

tell_food :: M ()
tell_food = do                                             -- Total: 30 = 29+1 + 1-1
    lnr <- get
    nextL $ \n -> mark _FOOD n                             -- 0:  tell others our mark
    senseAdjMove (lnr+7) (lnr+11) (lnr+11) Home            -- 1:  IF   an adjacent cell is my anthill and I moved there
    nextL $ \n -> drop n                                   -- 7:  THEN drop the food
    turnAround _GET_FOOD                                   -- 8:       turn around and return searching
    biasedMove (lnr+29)                                    -- 11: ELSE do one step of the random walk
    sense Here (lnr+7) lnr Home                            -- 29:      and check if we are accidentally home now (if so, drop and search, of not, try again)

get_food :: M ()
get_food = do                                                -- Total: 31 = 13+1 + 18-1
    lnr <- get
    senseAdjMoveAndNot (lnr+9) (lnr+13) (lnr+13) Food Home   -- 0:  IF    there is food (not on Home) and we moved to it
    nextL $ \n -> pickup n (lnr+13)                          -- 9:  THEN  pickup the food
    turnAround _RETURN_FOOD                                  -- 10:       turn around and bring the food to our home
    biasedMove lnr                                           -- 13: ELSE  do one step of a random walk and go on with what we do

return_food :: M ()
return_food = do                                             -- Total: 30 = 29+1 + 1-1
    lnr <- get
    nextL $ \n -> mark _FOOD n                             -- 0:  tell others our mark
    senseAdjMove (lnr+7) (lnr+11) (lnr+11) Home            -- 1:  IF   an adjacent cell is my anthill and I moved there
    nextL $ \n -> drop n                                   -- 7:  THEN drop the food
    turnAround _GET_FOOD                                   -- 8:       turn around and return searching
    biasedMove (lnr+29)                                    -- 11: ELSE do one step of the random walk
    sense Here (lnr+7) lnr Home                            -- 29:      and check if we are accidentally home now (if so, drop and search, of not, try again)

