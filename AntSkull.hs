{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}
module AntSkull where


-- A nice way to make our funtions
class Compile r where
    compile :: String -> r

instance Compile (IO ()) where
    compile s = putStrLn $ s

instance (Compile r, Show a) => Compile (a -> r) where
    compile s x = compile (s ++ " " ++ show x)


-- All functions are implemented using putStrLn's
data SenseDir  = Here | Ahead | LeftAhead | RightAhead deriving Show
data Condition = Friend | Foe | FriendWithFood | FoeWithFood
               | Food | Rock | Marker Mark | FoeMarker
               | Home | FoeHome deriving Show
data Turn      = Left | Right deriving Show
type Mark      = Int

sense :: SenseDir -> Int -> Int -> Condition -> IO ()
sense = compile "Sense"

mark :: Mark -> Int -> IO ()
mark = compile "Mark"

unmark :: Mark -> Int -> IO ()
unmark = compile "Unmark"

pickup :: Int -> Int -> IO ()
pickup = compile "PickUp"

drop :: Int -> IO ()
drop = compile "Drop"

turn :: Turn -> Int -> IO ()
turn = compile "Turn"

move :: Int -> Int -> IO ()
move = compile "Move"

random :: Int -> Int -> Int -> IO ()
random p = compile "Flip" p

