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
    _CheckDefend <- alloc
    _Defend      <- alloc

    -- We start by searching anything, food, enemies, whatever.               CURRENTLY FOOD ONLY
    search _Search _TellFoeHome _TellFood

    -- After we found anything, lets go tell the others what we found
    tellFoehome _TellFoeHome
    tellFood _TellFood _GetFood _ReturnFood _CheckDefend

    -- Now either continue to set a trap, or to get food
    getFood _GetFood _ReturnFood
    returnFood _ReturnFood _StoreFood
    storeFood _StoreFood _GetFood _Defend _ReturnFood
    checkDefend _CheckDefend _Defend _GetFood
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

    -- If we see Food (not on our Home), pick it up and _TellFood
    senseAdjMoveAndNot _checkFood _pickUp _randomWalk _randomWalk Food Home
    pickup _pickUp _turnAroundFood _randomWalk
    turnAround _turnAroundFood _TellFood

    -- Otherwise do a random walk and continue searching
    randomMove _randomWalk _this


tellFoehome :: Entry -> M ()
tellFoehome _this = do
    move _this 0 0


tellFood :: Entry -> Cont -> Cont -> Cont -> M ()
tellFood _this _GetFood _ReturnFood _CheckDefend = do
    _mark        <- alloc
    _checkHome   <- alloc
    _dropFood    <- alloc
    _followTrail <- alloc
    _checkAnyway <- alloc

    -- Check if there already is a food marker (if so, _ReturnFood)
    senseAdj _this _ReturnFood _mark (Marker _FOOD)

    -- If we didn't find another marker, mark the current spot
    mark _mark _FOOD _checkHome

    -- Check if we are home, if so, _StoreFood
    senseAdjMove _checkHome _dropFood _followTrail _followTrail Home
    drop _dropFood _CheckDefend

    -- If we did not find home or another food marker, return to home along our previous path
    tryFollowTrail _followTrail (Marker _PATH) _checkAnyway
    sense _checkAnyway Here _dropFood _this Home


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
    _randomDrop         <- alloc
    _dropAnyway         <- alloc

    -- COMMENT
    when _this (If RightAhead Food) _dropFoodRightAhead _randomDrop
    turn _dropFoodRightAhead Right _dropFoodAhead
    move _dropFoodAhead _dropFood _followHomeBorder
    drop _dropFood _GetFood

    -- With a small chance, drop food anyway
    random _randomDrop 5 _dropAnyway _followHomeBorder
    drop _dropAnyway _GetFood

    -- COMMENT
    followHomeBorder _followHomeBorder _this _ReturnFood


-- Follow a trail in front, or fail
followHomeBorder :: Entry -> Cont -> Cont -> M ()
followHomeBorder _this k1 k2 = do
    _turnLeft    <- alloc
    _checkRight  <- alloc
    _turnRight   <- alloc
    _moveForward <- alloc
    _ahead       <- alloc

    when _this (If LeftAhead Home) _turnLeft _ahead
    turn _turnLeft Left _ahead

    when _ahead (If Ahead Home) _moveForward _checkRight

    when _checkRight (If RightAhead Home) _turnRight k2
    turn _turnRight Right _moveForward
    move _moveForward k1 _checkRight


checkDefend :: Entry -> Cont -> Cont -> M ()
checkDefend _this _Defend _GetFood =  do
    _or2                  <- alloc
    _or3                  <- alloc
    _turnOr4              <- alloc
    _or4                  <- alloc
    _turnOr5              <- alloc
    _or5                  <- alloc
    _turnRightMoveForward <- alloc
    _turnLeftMoveForward  <- alloc
    _moveForward          <- alloc
    _turnAroundGetFood    <- alloc

    -- We are with our back to the opening -- check if there is one spot in the defence that needs reinforcement
    when _this (notIf Ahead Friend) _moveForward _or2
    when _or2 (notIf RightAhead Friend) _turnRightMoveForward _or3
    when _or3 (notIf LeftAhead Friend) _turnLeftMoveForward _turnOr4
    turn _turnOr4 Right _or4
    when _or4 (notIf RightAhead Friend) _turnRightMoveForward _turnOr5
    turn2 _turnOr5 Left _or5
    when _or5 (notIf LeftAhead Friend) _turnLeftMoveForward _turnAroundGetFood

    -- Reinforce the spot
    turn _turnRightMoveForward Right _moveForward
    turn _turnLeftMoveForward Left _moveForward
    move _moveForward _Defend _moveForward        -- POSSIBLE DEADLOCK (fix: also try moving backwards in the opening, or take another spot)

    -- If everything is defended, turn around and _GetFood
    turnAround _turnAroundGetFood _GetFood


defend :: Entry -> M ()
defend _this = do
    -- Infinite loop
    turn _this Right _this

