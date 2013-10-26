{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}
module AntSkull where

import Control.Monad.State hiding (when)
import Control.Monad.Writer hiding (when)
import Data.List (sortBy)
import Data.Function (on)
import Prelude hiding (Left, Right)


--
-- A nice way to print our functions
--
class Compile r where
    compile :: String -> Entry -> r

instance Compile (M () ) where
    compile s n = lift $ writer ((), [(n, s)])

instance (Compile r, Show a) => Compile (a -> r) where
    compile s n x = compile (s ++ " " ++ show x) n


type Entry = Int
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
tryFollowTrail :: Entry -> Mark -> Cont -> Cont -> M ()
tryFollowTrail n1 m k1 k2 = do
    n2 <- alloc
    n3 <- alloc
    n4 <- alloc
    n5 <- alloc
    return ()

    -- n1 RightAhead m

{-
-- Check a condition in all adjacent directions, and move to the corresponding place if the condition holds
-- Gets three state parameters (move succes, move fail and condition fail) and the condition
senseAdjMove :: Entry -> Int -> Int -> Int -> Condition -> StateT Int IO ()
senseAdjMove n k1 k2 k3 cond = do                  -- Total: 6
    lnr <- get
    nextL $ \n -> sense Ahead (lnr+5) n cond     -- 0: IF   the cell in front of me is COND
    nextL $ \n -> sense LeftAhead (lnr+3) n cond -- 1: OR   the cell left front of me is COND
    sense RightAhead (lnr+4) k3 cond             -- 2: OR   the cell left front of me is COND
    turn Left (lnr+5)                            -- 3:   (for the left case, turn left before continuing to the then)
    nextL $ \n -> turn Right n                   -- 4:   (for the right case, turn right before continuing to the then)
    nextL $ \n -> move k1 k2                     -- 5: THEN move onto COND
-}

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
senseAdj n1 k1 k2 cond = when n1 (If Ahead cond || If RightAhead cond || If LeftAhead cond) k1 k2

-- Check a condition in all adjacent directions, and move to the corresponding place if the condition holds
-- Gets three state parameters (move succes, move fail and condition fail) and the condition
senseAdjMove :: Int -> Int -> Int -> Condition -> StateT Int IO ()
senseAdjMove k1 k2 k3 cond = do                  -- Total: 6
    lnr <- get
    nextL $ \n -> sense Ahead (lnr+5) n cond     -- 0: IF   the cell in front of me is COND
    nextL $ \n -> sense LeftAhead (lnr+3) n cond -- 1: OR   the cell left front of me is COND
    sense RightAhead (lnr+4) k3 cond             -- 2: OR   the cell left front of me is COND
    turn Left (lnr+5)                            -- 3:   (for the left case, turn left before continuing to the then)
    nextL $ \n -> turn Right n                   -- 4:   (for the right case, turn right before continuing to the then)
    nextL $ \n -> move k1 k2                     -- 5: THEN move onto COND

-- Check a condition in all adjacent directions, and move to the corresponding place if the condition holds
-- Gets three state parameters (move succes, move fail and condition fail), the condition and the not-condition
senseAdjMoveAndNot :: Int -> Int -> Int -> Condition -> Condition -> StateT Int IO ()
senseAdjMoveAndNot k1 k2 k3 cond notCond = do              -- Total: 9
    lnr <- get
    nextL $ \n -> sense Ahead n (n+1) cond           -- 0: IF   the cell in front of me is COND
    nextL $ \n -> sense Ahead n (lnr+8) notCond      -- 1:      AND NOT notCond
    nextL $ \n -> sense LeftAhead n (n+1) cond       -- 2: OR   the cell left front of me is COND
    nextL $ \n -> sense LeftAhead n (lnr+6) notCond  -- 3:      AND NOT notCond
    nextL $ \n -> sense RightAhead n k3 cond         -- 4: OR   the cell left front of me is COND
    sense RightAhead k3 (lnr+7) notCond              -- 5:      AND NOT notCond
    turn Left (lnr+8)                                -- 6:   (for the left case, turn left before continuing to the then)
    nextL $ \n -> turn Right n                       -- 7:   (for the right case, turn right before continuing to the then)
    nextL $ \n -> move k1 k2                         -- 8: THEN move onto COND

-- Turn multiple times
-- Gets the nurmal turn parameters: a turn direction {Left, Right} and the state paramweter
turn2 :: Turn -> Int -> StateT Int IO ()
turn2 t k = nextL $ \n -> turn t n >> turn t k                                   -- Total: 2

turnAround :: Int -> StateT Int IO ()
turnAround k = nextL $ \n -> turn Right n >> turn Right (n+1) >> turn Right k    -- Total: 3

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
random :: Float -> Int -> Int -> StateT Int IO ()
random 10 k1 k2 = rand 10 k1 k2        -- Total: varies
random 33 k1 k2 = rand 3 k1 k2
random 25 k1 k2 = rand 4 k1 k2
random 67 k1 k2 = rand 3 k2 k1
random 75 k1 k2 = rand 4 k2 k1
random 90 k1 k2 = rand 10 k2 k1
random p k1 k2  = do
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

