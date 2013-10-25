{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}
module AntSkull where

import Control.Monad.State
import Prelude hiding (Left, Right)


--
-- A nice way to print our functions
--
class Compile r where
    compile :: String -> r

instance Compile String where
    compile s = s

instance Compile (StateT Int IO ()) where
    compile s = do
        n <- get
        modify (+1)
        liftIO $ putStrLn (s ++ "      ;" ++ show n)

instance (Compile r, Show a) => Compile (a -> r) where
    compile s x = compile (s ++ " " ++ show x)

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
sense :: SenseDir -> Int -> Int -> Condition -> StateT Int IO ()
sense = compile "Sense"

mark :: Mark -> Int -> StateT Int IO ()
mark = compile "Mark"

unmark :: Mark -> Int -> StateT Int IO ()
unmark = compile "Unmark"

pickup :: Int -> Int -> StateT Int IO ()
pickup = compile "PickUp"

drop :: Int -> StateT Int IO ()
drop = compile "Drop"

turn :: Turn -> Int -> StateT Int IO ()
turn = compile "Turn"

move :: Int -> Int -> StateT Int IO ()
move = compile "Move"

rand :: Int -> Int -> Int -> StateT Int IO ()
rand = compile "Flip"

comment :: String -> StateT Int IO ()
comment s = liftIO $ putStrLn ("; " ++ s)


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
-- Gets three state parameters (move succes, move fail and condition fail) and the condition
senseAdjMoveAndNot :: Int -> Int -> Int -> Condition -> Condition -> StateT Int IO ()
senseAdjMove k1 k2 k3 cond = do                  -- Total: 6
    lnr <- get
    nextL $ \n -> sense Ahead (lnr+5) n cond     -- 0: IF   the cell in front of me is COND
    nextL $ \n -> sense LeftAhead (lnr+3) n cond -- 1: OR   the cell left front of me is COND
    sense RightAhead (lnr+4) k3 cond             -- 2: OR   the cell left front of me is COND
    turn Left (lnr+5)                            -- 3:   (for the left case, turn left before continuing to the then)
    nextL $ \n -> turn Right n                   -- 4:   (for the right case, turn right before continuing to the then)
    nextL $ \n -> move k1 k2                     -- 5: THEN move onto COND

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

-- Create a decent randomizer in terms of the Flip randomizer
-- Gets its current line number, the percentage in [0, 100], and the two state parameters
random :: Float -> Int -> Int -> StateT Int IO ()
random 33 k1 k2 = rand 3 k1 k2        -- Total: varies
random 25 k1 k2 = rand 4 k1 k2
random 67 k1 k2 = rand 3 k2 k1
random 75 k1 k2 = rand 4 k2 k1
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


--
-- Some functions to help managing line numbers
--
nextL f = get >>= \n -> f (n+1)    -- @Chiel, are you sure these functions are right this way?
curL f = get >>= f

