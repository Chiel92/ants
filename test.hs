{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}
module AntSkull where

import Control.Monad.State
import Control.Monad.Writer
import Data.List (sortBy)
import Data.Function (on)
import Prelude hiding (Left, Right)


--
-- A nice way to print our functions
--
class Compile r where
    compile :: String -> Entry -> r

instance Compile (StateT Int (Writer Program) () ) where
    compile s n = lift $ writer ((), [(n, s)])

instance (Compile r, Show a) => Compile (a -> r) where
    compile s n x = compile (s ++ " " ++ show x) n


type Entry = Int
type Cont = Int
type Program = [(Entry, String)]


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
sense :: Entry -> SenseDir -> Int -> Int -> Condition -> StateT Int (Writer Program) ()
sense n = compile "Sense" n

mark :: Entry -> Mark -> Int -> StateT Int (Writer Program) ()
mark n = compile "Mark" n

unmark :: Entry -> Mark -> Int -> StateT Int (Writer Program) ()
unmark n = compile "Unmark" n

pickup :: Entry -> Int -> Int -> StateT Int (Writer Program) ()
pickup n = compile "PickUp" n

drop :: Entry -> Int -> StateT Int (Writer Program) ()
drop n = compile "Drop" n

turn :: Entry -> Turn -> Cont -> StateT Int (Writer Program) ()
turn n = compile "Turn" n

move :: Entry -> Cont -> Cont -> StateT Int (Writer Program) ()
move n = compile "Move" n

rand :: Entry -> Int -> Cont -> Cont -> StateT Int (Writer Program) ()
rand n = compile "Flip" n


--shift :: StateT Int (Writer Program) ()
--shift = do
--    n <- get
--    modify (+1)
--    return n

-- print program to IO
main = do
    mapM_ (putStrLn . snd) (run program')
    --mapM_ print (run program')

-- generate the program and sort instructions on line number
run program = sortBy (compare `on` fst) (snd $ runWriter (runStateT program 1))

-- an example program
program = randomMove' 0 0
program' = biasedMove' 0 0



-- Do a random walk (for one step)
-- GEEF GLOBALS DOOR ALS PARAMETERS
randomMove' :: Entry -> Cont -> StateT Int (Writer Program) ()
randomMove' n k = do
    --(n1,n2,n3,n4,n5) <- return (n+1, n+2, n+3, n+4, n+5)
    --modify (+5)
    -- n1 <- shift
    -- n2 <- shift
    -- n3 <- shift
    -- n4 <- shift
    -- n5 <- shift
    n2 <- get
    modify (+1)
    n3 <- get
    modify (+1)
    n4 <- get
    modify (+1)
    n5 <- get
    modify (+1)

    rand n 2 n5 n2
    rand n2 2 n3 n4
    turn n3 Right n5
    turn n4 Left n5
    move n5 k n

-- Do a random move, but with a high chance of following markers (for one step)
biasedMove' :: Entry -> Cont -> StateT Int (Writer Program) ()
biasedMove' n k = do
    n2 <- get
    modify (+1)
    n3 <- get
    modify (+1)
    n4 <- get
    modify (+1)

    rand n 4 n2 n3
    turn n2 Right k
    randomMove' n3 k
    turn n4 Left k


{-
--
-- Our extension functions
--
-- Check a condition in all adjacent directions
-- Gets the two state parameters and the condition
senseAdj :: Int -> Int -> Condition -> StateT Int IO ()
senseAdj k1 k2 cond = do                         -- Total: 3
    lnr <- get
    sense Ahead k1 (lnr+1) cond
    sense LeftAhead k1 (lnr+2) cond
    sense RightAhead k1 k2 cond

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

-}

--
-- Some functions to help managing line numbers
--
nextL f = get >>= \n -> f (n+1)    -- @Chiel, are you sure these functions are right this way?
curL f = get >>= f

