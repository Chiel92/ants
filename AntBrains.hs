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
    tellFood _TellFood _GetFood _ReturnFood _StoreFood

    -- Now either continue to set a trap, or to get food
    getFood _GetFood _ReturnFood
    returnFood _ReturnFood _StoreFood
    storeFood _StoreFood _GetFood _Defend _ReturnFood
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


tellFood :: Entry -> Entry -> Entry -> Entry -> M ()
tellFood _this _GetFood _ReturnFood _StoreFood = do
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

     -- If we did not find home or another food marker, do the random walk
    randomMove _randomWalk _this


getFood :: Entry -> Entry -> M ()
getFood _this _ReturnFood = do
    _pickUp         <- alloc
    _turnAround     <- alloc
    _tryFollowTrail <- alloc 

    -- Check if we found food, if so, get it, turn around and _ReturnFood
    senseAdjMoveAndNot _this _pickUp _randomWalk _randomWalk Food Home
    pickup _pickUp _turnAround _tryFollowTrail
    turnAround _turnAround _ReturnFood

    -- If we didn't find food, follow the trail to the food
    tryFollowTrail _tryFollowTrail (Marker _FOOD) _this
    

returnFood :: Entry -> Entry -> M ()
returnFood _this _StoreFood = do
    _tryFollowTrail <- alloc

    -- Check if we found home, if so _StoreFood
    senseAdj _this _StoreFood _tryFollowTrail _tryFollowTrail Home

    -- If we didn't find home, follow the trail
    tryFollowTrail _tryFollowTrail _this

