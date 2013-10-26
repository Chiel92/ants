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
    _StoreFood   <- alloc
    _Defend      <- alloc

    -- We start by searching anything, food, enemies, whatever.               CURRENTLY FOOD ONLY
    search _Search _TellFood _TellFoehome
    -- After we found anything, lets go tell the others what we found
    tellFoehome _TellFoehome
    tellFood _TellFood _GetFood _ReturnFood
    -- Now either continue to set a trap, or to get food
    getFood _GetFood _ReturnFood
    returnFood _ReturnFood _GetFood
    storeFood _StoreFood _GetFood
    defend _Defend


-- The implementation functions for our strategy
search :: Entry -> Entry -> Entry -> M ()
search _this _TellFood _TellFoehome = do
    _pickUp     <- alloc
    _turnAround <- alloc
    _randomWalk <- alloc

    -- If we see Food (not on our Home), pick it up and TELL_FOOD
    senseAdjMoveAndNot _this _pickUp _randomWalk _randomWalk Food Home
    pickup _pickUp _turnAround _randomWalk
    turnAround _turnAround _TellFood

    -- Otherwise do a random walk and continue searching
    randomMove _randomWalk _this


tellFoehome :: Entry -> M ()
tellFoehome _this = do
    move _this 0 0


tellFood :: Entry -> Entry -> Entry -> M ()
tellFood _this _GetFood _ReturnFood = do
    _checkExistingMarker <- alloc
    _checkHome           <- alloc
    _dropFood            <- alloc
    _randomWalk          <- alloc

    -- Mark the current spot
    mark _this _FOOD _checkExistingMarker

    -- Check if there already is a food marker (if so, _ReturnFood)
    senseAdj _checkExistingMarker _ReturnFood _checkHome (Marker _FOOD)

    -- Check if we are home, if so, _StoreFood
    senseAdj _checkHome _StoreFood _randomWalk _randomWalk Home

     -- Do the random walk
    randomMove _randomWalk _this


getFood :: Entry -> Entry -> M ()
getFood _this _ReturnFood = do
    lnr <- get
    senseAdjMoveAndNot (lnr+9) (lnr+13) (lnr+13) Food Home   -- 0:  IF    there is food (not on Home) and we moved to it
    nextL $ \n -> pickup n (lnr+13)                          -- 9:  THEN  pickup the food
    turnAround _RETURN_FOOD                                  -- 10:       turn around and bring the food to our home
    biasedMove lnr                                           -- 13: ELSE  do one step of a random walk and go on with what we do


returnFood :: Entrt -> Entry -> M ()
returnFood _this _GetFood = do
    lnr <- get
    nextL $ \n -> mark _FOOD n                             -- 0:  tell others our mark
    senseAdjMove (lnr+7) (lnr+11) (lnr+11) Home            -- 1:  IF   an adjacent cell is my anthill and I moved there
    nextL $ \n -> drop n                                   -- 7:  THEN drop the food
    turnAround _GET_FOOD                                   -- 8:       turn around and return searching
    biasedMove (lnr+29)                                    -- 11: ELSE do one step of the random walk
    sense Here (lnr+7) lnr Home                            -- 29:      and check if we are accidentally home now (if so, drop and search, of not, try again)

