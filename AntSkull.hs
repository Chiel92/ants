{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}
module AntSkull where

import Control.Monad.State hiding (when)
import Control.Monad.Writer hiding (when)
import Data.List (sortBy)
import Data.Function (on)
import Prelude hiding (Left, Right, (&&), (||))


--
-- A nice way to print our functions
--
class Compile r where
    compile :: String -> Entry -> r

instance Compile (M () ) where
    compile s n = lift $ writer ((), [(n, s)])

instance (Compile r, Show a) => Compile (a -> r) where
    compile s n x = compile (s ++ " " ++ show x) n


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
main = do
    debug $ program 0
    --mapM_ print (run program)

-- Generate the program and sort instructions on line number
run :: M () -> Program
run prog = sortBy (compare `on` fst) (snd $ runWriter (runStateT (alloc >> prog) 0))

-- Print function to IO
debug :: M () -> IO ()
debug prog = mapM_ (putStrLn . snd) (run prog)

-- An example program
program :: Entry -> M ()
program n1 = randomMove n1 0


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
sense :: Entry -> SenseDir -> Int -> Int -> Condition -> M ()
sense n = compile "Sense" n

mark :: Entry -> Mark -> Int -> M ()
mark n = compile "Mark" n

unmark :: Entry -> Mark -> Int -> M ()
unmark n = compile "Unmark" n

pickup :: Entry -> Int -> Int -> M ()
pickup n = compile "PickUp" n

drop :: Entry -> Int -> M ()
drop n = compile "Drop" n

turn :: Entry -> Turn -> Cont -> M ()
turn n = compile "Turn" n

move :: Entry -> Cont -> Cont -> M ()
move n = compile "Move" n

rand :: Entry -> Int -> Cont -> Cont -> M ()
rand n = compile "Flip" n


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

-- Do a random walk (for one step)
randomMove :: Entry -> Cont -> M ()
randomMove _this k = do
    n2 <- alloc
    n3 <- alloc
    n4 <- alloc
    n5 <- alloc

    rand _this 2 n5 n2
    rand n2 2 n3 n4
    turn n3 Right n5
    turn n4 Left n5
    move n5 k _this


tryFollowTrail :: Entry -> Mark -> Cont -> Cont -> M ()
tryFollowTrail _this m k1 k2 = do
    _turnLeft    <- alloc
    _checkRight  <- alloc
    _moveForward <- alloc
    _moveOnTrail <- alloc

    when _this (If Ahead (Marker m)) _turnLeft _checkRight
    turn _turnLeft Left _this
    when _checkRight (If RightAhead (Marker m)) _moveForward k2
    move _moveForward k1 _moveOnTrail

    turn _moveOnTrail Right _moveForward

-- Do a random walk (for one step)
-- GEEF GLOBALS DOOR ALS PARAMETERS
randomMove :: Entry -> Cont -> M ()
randomMove n1 k = do
    n2 <- alloc
    n3 <- alloc
    n4 <- alloc
    n5 <- alloc

    rand n1 2 n5 n2
    rand n2 2 n3 n4
    turn n3 Right n5
    turn n4 Left n5
    move n5 k n1

-- Check a condition in all adjacent directions
-- Gets the two state parameters and the condition
senseAdj :: Entry -> Cont -> Cont -> Condition -> StateT Int IO ()
senseAdj _this k1 k2 cond = when _this (If Ahead cond || If RightAhead cond || If LeftAhead cond) k1 k2

-- Check a condition in all adjacent directions, and move to the corresponding place if the condition holds
-- Gets three state parameters (move succes, move fail and condition fail) and the condition
senseAdjMove :: Entry -> Cont -> Cont -> Cont -> Condition -> M ()
senseAdjMove _this k1 k2 k3 cond = do
    _or2   <- alloc
    _or3   <- alloc
    _move  <- alloc
    _turnR <- alloc
    _turnL <- alloc

    when _this (If Ahead cond) _move _or2
    when _or2 (If RightAhead cond) _turnR _or3
    when _or3 (If LeftAhead cond) _turnL k3
    turn _turnR Right _move
    turn _turnL Left _move
    move _move k1 k2

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

-- Turn multiple times
-- Gets the nurmal turn parameters: a turn direction {Left, Right} and the state paramweter
turn2 :: Entry -> Turn -> Cont -> M ()
turn2 _this t k = do
    _t2 <- alloc

    turn _this t _t2
    turn _t2 t k

turnAround :: Int -> StateT Int IO ()
turnAround k = do
    _t2 <- alloc
    _t3 <- alloc

    turn _this t _t2
    turn _t2 t _t3
    turn _t3 t k

-- Do a random walk (for one step)
randomMove :: Int -> StateT Int IO ()
randomMove k = do         -- Total: 5
    n <- get
    random 75 (n+4) (n+1) -- n
    random 50 (n+2) (n+3) -- n+1
    turn Right (n+4)      -- n+2
    turn Left (n+4)       -- n+3
    move k n              -- n+4

-- Do a random move, but with a high chance of following markers (for one step)
biasedMove :: Int -> StateT Int IO ()
biasedMove k = do                                     -- Total: 18 = 13+1 + 5-1
    lnr <- get
    random 90 (lnr+1) (lnr+13)                        -- 0:
    senseAdjMove k (lnr+7) (lnr+7) (Marker 0)         -- 1:
    senseAdjMove k (lnr+13) (lnr+13) (Marker 1)       -- 7:
    randomMove k                                      -- 13:


-- Create a decent randomizer in terms of the Flip randomizer
-- Gets its current line number, the percentage in [0, 100], and the two state parameters
random :: Entry -> Float -> Cont -> Cont -> M ()
random _this 10 k1 k2 = rand _this 10 k1 k2
random _this 33 k1 k2 = rand _this 3 k1 k2
random _this 25 k1 k2 = rand _this 4 k1 k2
random _this 67 k1 k2 = rand _this 3 k2 k1
random _this 75 k1 k2 = rand _this 4 k2 k1
random _this 90 k1 k2 = rand _this 10 k2 k1
random _this 50 k1 k2 = rand _this 2 k1 k2
{-
random _this p k1 k2  = do
    lnr <- get
    rtree lnr p 50 1
      where
        rtree :: Int -> Float -> Float -> Int -> StateT Int IO ()
        rtree lnr p q d | close p q = rand 2 k1 k2
                        | otherwise = if p < q
                                      then do
                                          rand 2 (lnr+d) k2
                                          rtree lnr p (q - getP (d+1)) (d+1)
                                      else do
                                          rand 2 k1 (lnr+d)
                                          rtree lnr p (q + getP (d+1)) (d+1)

        -- Check if the current probability is close enough to the wanted probability
        close :: Float -> Float -> Bool
        close goalP currentP = abs (goalP - currentP) < 1

        -- Get the probability corresponding to the depth of the tree
        getP :: Int -> Float
        getP d = 100 / (2 ** (fromIntegral d))
-}

