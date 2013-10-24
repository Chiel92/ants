{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}
module AntSkull where

import Control.Monad.State
import Prelude hiding (Left, Right)


-- A nice way to print our functions
class Compile r where
    compile :: String -> r

instance Compile String where
    compile s = s

instance Compile (StateT Int IO ()) where
    compile s =
      do
        modify (+1)
        liftIO $ putStrLn s

instance (Compile r, Show a) => Compile (a -> r) where
    compile s x = compile (s ++ " " ++ show x)


-- Some datatypes to make life more beautiful
data SenseDir  = Here | Ahead | LeftAhead | RightAhead deriving Show
data Condition = Friend | Foe | FriendWithFood | FoeWithFood
               | Food | Rock | Marker Mark | FoeMarker
               | Home | FoeHome deriving Show
data Turn      = Left | Right deriving Show
type Mark      = Int


-- The primitive functions
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


-- Our extension functions

randomMove :: Int -> StateT Int IO ()
randomMove k =
  do
    n <- get
    random 50 (n+4) (n+1) -- n
    random 50 (n+2) (n+3) -- n+1
    turn Right (n+4)      -- n+2
    turn Left (n+4)       -- n+3
    move n k              -- n+4

-- Create a decent randomizer in terms of the Flip randomizer
-- Gets its current line number, the percentage in [0, 100], and the two state parameters
random :: Float -> Int -> Int -> StateT Int IO ()
random p k1 k2 =
  do
    n <- get
    rtree n p 50 1
      where
        rtree :: Int -> Float -> Float -> Int -> StateT Int IO ()
        rtree lineNr p q d | close p q = rand 2 k1 k2
                           | otherwise = if p < q
                                  then do
                                      rand 2 (lineNr+d) k2
                                      rtree lineNr p (q - getP (d+1)) (d+1)
                                  else do
                                      rand 2 k1 (lineNr+d)
                                      rtree lineNr p (q + getP (d+1)) (d+1)

        -- Check if the current probability is close enough to the wanted probability
        close :: Float -> Float -> Bool
        close goalP currentP = abs (goalP - currentP) < 1

        -- Get the probability corresponding to the depth of the tree
        getP :: Int -> Float
        getP d = 100 / (2 ** (fromIntegral d))

-- Check a condition in all adjacent directions
-- Gets its current line number, the two state parameters and the condition
senseAdj :: Int -> Int -> Int -> Condition -> StateT Int IO ()
senseAdj lineNr k1 k2 cond = do -- I NEED MY CURRENT LINE NUMBER!!!
    sense Ahead k1 (lineNr+1) cond
    sense LeftAhead k1 (lineNr+1) cond
    sense RightAhead k1 k2 cond

-- Turn multiple times
-- Gets the nurmal turn parameters: a turn direction {Left, Right} and the state paramweter
turn2 :: Turn -> Int -> StateT Int IO ()
turn2 t k = turn t nextLnr >> turn t k

turnAround :: Int -> StateT Int IO ()
turnAround k = turn AntSkull.Right nextLnr >> turn AntSkull.Right nextLnr >> turn AntSkull.Right k

nextLnr = undefined
