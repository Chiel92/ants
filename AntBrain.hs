module AntBrain where

import AntSkull
import Prelude hiding (drop, Right, Left, (&&), (||))


-- The markers
_FOEHOME = 0
_FOOD    = 1
_PATH    = 5


main = debug $ program 0

test _this = do
    search 0 0 0

-- Our strategy
program :: Entry -> M ()
program _Search = do
    _TellFoeHome <- alloc
    _TellFood    <- alloc
    _GetFood     <- alloc
    _ReturnFood  <- alloc
    _StoreFood   <- alloc
    _Defend      <- alloc

    -- We start by searching anything, food, enemies, whatever.               CURRENTLY FOOD ONLY
    search _Search _TellFoeHome _TellFood

    -- After we found anything, lets go tell the others what we found
    tellFoehome _TellFoeHome
    tellFood _TellFood _GetFood _ReturnFood _StoreFood

    -- Now either continue to set a trap, or to get food
    getFood _GetFood _ReturnFood
    returnFood _ReturnFood _StoreFood
    storeFood _StoreFood _GetFood _Defend _ReturnFood
    defend _Defend


-- The implementation functions for our strategy
search :: Entry -> Cont -> Cont -> M ()
search _this _TellFoeHome _TellFood = do
    _checkFoe       <- alloc
    _turnAroundFoe  <- alloc
    _checkFood      <- alloc
    _pickUp         <- alloc
    _turnAroundFood <- alloc
    _randomWalk     <- alloc

    -- Mark our current path
    mark _this _PATH _checkFoe

    -- If se see the FoeHome, run away and _TellFoeHome
    senseAdj _checkFoe _turnAroundFoe _checkFood FoeHome
    turnAround _turnAroundFoe _TellFoeHome

    -- If we see Food (not on our Home), pick it up and TELL_FOOD
    senseAdjMoveAndNot _checkFood _pickUp _randomWalk _randomWalk Food Home
    pickup _pickUp _turnAroundFood _randomWalk
    turnAround _turnAroundFood _TellFood

    -- Otherwise do a random walk and continue searching
    randomMove _randomWalk _this


tellFoehome :: Entry -> M ()
tellFoehome _this = do
    move _this 0 0


tellFood :: Entry -> Cont -> Cont -> Cont -> M ()
tellFood _this _GetFood _ReturnFood _StoreFood = do
    _mark        <- alloc
    _checkHome   <- alloc
    _followTrail <- alloc

    -- Check if there already is a food marker (if so, _ReturnFood)
    senseAdj _this _ReturnFood _mark (Marker _FOOD)

    -- If we didn't find another marker, mark the current spot
    mark _mark _FOOD _checkHome

    -- Check if we are home, if so, _StoreFood
    senseAdj _checkHome _StoreFood _followTrail Home

    -- If we did not find home or another food marker, return to home along our previous path
    tryFollowTrail _followTrail (Marker _PATH) _this


getFood :: Entry -> Cont -> M ()
getFood _this _ReturnFood = do
    _pickUp         <- alloc
    _turnAround     <- alloc
    _tryFollowTrail <- alloc 

    -- Check if we found food, if so, get it, turn around and _ReturnFood
    senseAdjMoveAndNot _this _pickUp _tryFollowTrail _tryFollowTrail Food Home
    pickup _pickUp _turnAround _tryFollowTrail
    turnAround _turnAround _ReturnFood

    -- If we didn't find food, follow the trail to the food
    tryFollowTrail _tryFollowTrail (Marker _FOOD) _this


returnFood :: Entry -> Cont -> M ()
returnFood _this _StoreFood = do
    _tryFollowTrail <- alloc

    -- Check if we found home, if so _StoreFood
    senseAdj _this _StoreFood _tryFollowTrail Home

    -- If we didn't find home, follow the trail
    tryFollowTrail _tryFollowTrail (Marker _FOOD) _this


storeFood :: Entry -> Cont -> Cont -> Cont -> M ()
storeFood _this _GetFood _Defend _ReturnFood = do
    _dropFoodRightAhead <- alloc
    _dropFoodAhead      <- alloc
    _dropFood           <- alloc
    _followHomeBorder   <- alloc

    -- COMMENT
    when _this (If RightAhead Food) _dropFoodRightAhead _followHomeBorder
    turn _dropFoodRightAhead Right _dropFoodAhead
    move _dropFoodAhead _dropFood _followHomeBorder
    drop _dropFood _GetFood

    -- COMMENT
    followHomeBorder _followHomeBorder _this _ReturnFood


-- Follow a trail in front, or fail
followHomeBorder :: Entry -> Cont -> Cont -> M ()
followHomeBorder _this k1 k2 = do
    _turnLeft        <- alloc
    _checkRight      <- alloc
    _checkRightAgain <- alloc
    _turnRight       <- alloc
    _moveForward     <- alloc
    _turnToTrail     <- alloc
    _moveOnTrail     <- alloc
    _turnBackLeft    <- alloc

    -- Turn left till no marker is ahead, then _checkRight
    when _this (If Ahead Home) _moveForward _checkRight
    turn _turnLeft Left _this

    -- Try follow a trail (and turn right, if needed), else _turnToTrail
    when _checkRight (If RightAhead Home) _moveForward _turnRight
    turn _turnRight Right _checkRightAgain
    when _checkRightAgain (If RightAhead Home) _moveForward k2
    move _moveForward k1 _turnToTrail

    -- Move on the trail if we can't go forward
    turn _turnToTrail Right _moveOnTrail
    move _moveOnTrail _turnBackLeft _turnRight
    turn _turnBackLeft Left k1


defend :: Entry -> M ()
defend _this = do
    move _this 0 0

