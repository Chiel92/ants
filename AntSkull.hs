{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}
module AntSkull where

import Control.Monad.State hiding (when)
import Control.Monad.Writer hiding (when)
import Data.List (sortBy)
import Data.Function (on)
import Prelude hiding (drop, Left, Right, (&&), (||))


--
-- A nice way to print our functions
--
class PrimitiveInstruction r where
    instruction :: String -> Entry -> r

instance PrimitiveInstruction (M ()) where
    instruction s n = lift $ writer ((), [(n, s)])

instance (PrimitiveInstruction r, Show a) => PrimitiveInstruction (a -> r) where
    instruction s n x = instruction (s ++ " " ++ show x) n


type Entry = Int -- Convention: all entry identifiers start with an underscore
type Cont = Int
type Program = [(Entry, String)]
type M = StateT Int (Writer Program)

-- Allocate an entrypoint for a function
alloc :: M Int
alloc = do
    n <- get
    modify (+1)
    return n


--
-- IO functionality
--

-- Generate the program and sort instructions on line number
run :: M () -> Program
run prog = sortBy (compare `on` fst) (snd $ runWriter (runStateT (alloc >> prog) 0))

-- Print function to IO
debug :: M () -> IO ()
debug prog = mapM_ (putStrLn . snd) (run prog)


--
-- Some datatypes to make life more beautiful
--
data SenseDir  = Here | Ahead | LeftAhead | RightAhead deriving Show
data Condition = Friend | Foe | FriendWithFood | FoeWithFood
               | Food | Rock | Marker Mark | FoeMarker
               | Home | FoeHome deriving Show
data Turn      = Left | Right deriving Show
type Mark      = Int


--
-- The primitive functions
--
sense :: Entry -> SenseDir -> Cont -> Cont -> Condition -> M ()
sense n = instruction "Sense" n

mark :: Entry -> Mark -> Cont -> M ()
mark n = instruction "Mark" n

unmark :: Entry -> Mark -> Cont -> M ()
unmark n = instruction "Unmark" n

pickup :: Entry -> Cont -> Cont -> M ()
pickup n = instruction "PickUp" n

drop :: Entry -> Cont -> M ()
drop n = instruction "Drop" n

turn :: Entry -> Turn -> Cont -> M ()
turn n = instruction "Turn" n

move :: Entry -> Cont -> Cont -> M ()
move n = instruction "Move" n

rand :: Entry -> Int -> Cont -> Cont -> M ()
rand n = instruction "Flip" n


--
-- Advanced sensing system
--
data Expr = If SenseDir Condition | Not Expr | And Expr Expr | Or Expr Expr

notIf dir cond   = Not (If dir cond)
(&&) expr1 expr2 = And expr1 expr2
(||) expr1 expr2 = Or expr1 expr2

when :: Entry -> Expr -> Cont -> Cont -> M ()
when n1 (If dir cond) k1 k2 = sense n1 dir k1 k2 cond
when n1 (Not expr) k1 k2 = when n1 expr k2 k1
when n1 (And expr1 expr2) k1 k2 = do
    n2 <- alloc
    when n1 expr1 n2 k2
    when n2 expr2 k1 k2
when n1 (Or expr1 expr2) k1 k2 = do
    n2 <- alloc
    when n1 expr1 k1 n2
    when n2 expr2 k1 k2


--
-- Our extension functions
--
-- GEEF GLOBALS DOOR ALS PARAMETERS

-- Do a random move (biased to go fairly straight)
randomMove :: Entry -> Cont -> M ()
randomMove _this k = do
    _rand  <- alloc
    _turnR <- alloc
    _turnL <- alloc
    _move  <- alloc

    random _this 75 _move _rand
    random _rand 50 _turnR _turnL
    turn _turnR Right _move
    turn _turnL Left _move
    move _move k _this

-- Try follow a trail in front, else do a random move
tryFollowTrail :: Entry -> Condition -> Cont -> M ()
tryFollowTrail _this cond k = do
    _randomMove <- alloc

    followTrail _this cond k _randomMove
    randomMove _randomMove k

-- Follow a trail in front, or fail
followTrail :: Entry -> Condition -> Cont -> Cont -> M ()
followTrail _this cond k1 k2 = do
    _moveAgain  <- alloc
    _moveAround <- alloc

    -- If the marker is in front of us and we moved there, go to k1
    senseAdjMove _this k1 _moveAgain k2 cond
    move21 _moveAgain k1 _moveAround
    
    -- If we keep bumping into something, move around
    moveAround _moveAround k1 k2

-- Try to pass at the right side
moveAround :: Entry -> Cont -> Cont -> M ()
moveAround _this k1 k2 = do
    _moveForwardRight  <- alloc
    _turnBackRight     <- alloc
    _turnLeft          <- alloc
    _moveForwardLeft   <- alloc
    _turnBackLeft      <- alloc
    _moveLeftFailed    <- alloc

    -- Turn right, move forward and turn back left
    turn _this Right _moveForwardRight
    move _moveForwardRight _turnBackRight _turnLeft
    turn2 _turnBackRight Left k1

    -- If the move failed, try to pass at the left side
    turn2 _turnLeft Left _moveForwardLeft
    move _moveForwardLeft _turnBackLeft _moveLeftFailed
    turn2 _turnBackLeft Right k1

    -- If that failed as well, then turn back and fail for real
    turn _moveLeftFailed Right k2


-- Check a condition in all adjacent directions
-- Gets the two state parameters and the condition
senseAdj :: Entry -> Cont -> Cont -> Condition -> M ()
senseAdj _this k1 k2 cond = when _this (If Ahead cond || If RightAhead cond || If LeftAhead cond) k1 k2

-- Check a condition in all adjacent directions, and move to the corresponding place if the condition holds
-- Gets three state parameters (move succes, move fail and condition fail) and the condition
senseAdjMove :: Entry -> Cont -> Cont -> Cont -> Condition -> M ()
senseAdjMove _this k1 k2 k3 cond = do
    _or2   <- alloc
    _or3   <- alloc
    _turnR <- alloc
    _turnL <- alloc
    _moveA <- alloc
    _moveR <- alloc
    _moveL <- alloc

    when _this (If Ahead cond) _moveA _or2
    when _or2 (If RightAhead cond) _turnR _or3
    when _or3 (If LeftAhead cond) _turnL k3
    move _moveA k1 _or2
    turn _turnR Right _moveR
    move _moveR k1 _or3
    turn _turnL Left _moveL
    move _moveL k1 k2

-- Check a condition in all adjacent directions, and move to the corresponding place if the condition holds
-- Gets three state parameters (move succes, move fail and condition fail), the condition and the not-condition
senseAdjMoveAndNot :: Entry -> Cont -> Cont -> Cont -> Condition -> Condition -> M ()
senseAdjMoveAndNot _this k1 k2 k3 cond notCond = do
    _or2   <- alloc
    _or3   <- alloc
    _move  <- alloc
    _turnR <- alloc
    _turnL <- alloc

    when _this (If Ahead cond && notIf Ahead notCond) _move _or2
    when _or2 (If RightAhead cond && notIf Ahead notCond) _turnR _or3
    when _or3 (If LeftAhead cond && notIf Ahead notCond) _turnL k3
    turn _turnR Right _move
    turn _turnL Left _move
    move _move k1 k2

-- Move multiple times
-- Gets the normal move parameters
move7 :: Entry -> Cont -> Cont -> M ()
move7 _this k1 k2 = do
    _m2 <- alloc
    _m3 <- alloc
    _m4 <- alloc
    _m5 <- alloc
    _m6 <- alloc
    _m7 <- alloc

    move _this k1 _m2
    move _m2 k1 _m3
    move _m3 k1 _m4
    move _m4 k1 _m5
    move _m5 k1 _m6
    move _m6 k1 _m7
    move _m7 k1 k2

move21 :: Entry -> Cont -> Cont -> M()
move21 _this k1 k2 = do
    _m8  <- alloc
    _m15 <- alloc
    
    move7 _this k1 _m8
    move7 _m8 k1 _m15
    move7 _m15 k1 k2

-- Turn multiple times
-- Gets the normal turn parameters: a turn direction {Left, Right} and the state paramweter
turn2 :: Entry -> Turn -> Cont -> M ()
turn2 _this t k = do
    _t2 <- alloc

    turn _this t _t2
    turn _t2 t k

turnAround :: Entry -> Cont -> M ()
turnAround _this k = do
    _t2 <- alloc
    _t3 <- alloc

    turn _this Right _t2
    turn _t2 Right _t3
    turn _t3 Right k

-- Create a decent randomizer in terms of the Flip randomizer
-- Gets its current line number, the percentage in [0, 100], and the two state parameters
random :: Entry -> Float -> Cont -> Cont -> M ()
random _this 5 k1 k2 = rand _this 20 k1 k2
random _this 10 k1 k2 = rand _this 10 k1 k2
random _this 33 k1 k2 = rand _this 3 k1 k2
random _this 25 k1 k2 = rand _this 4 k1 k2
random _this 50 k1 k2 = rand _this 2 k1 k2
random _this 67 k1 k2 = rand _this 3 k2 k1
random _this 75 k1 k2 = rand _this 4 k2 k1
random _this 90 k1 k2 = rand _this 10 k2 k1

