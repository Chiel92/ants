{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}
module AntSkull where


-- A nice way to print our funtions
class Compile r where
    compile :: String -> r

instance Compile String where
    compile s = s

instance Compile (IO ()) where
    compile s = putStrLn $ s

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

rand :: Int -> Int -> Int -> IO ()
rand = compile "Flip"


-- Our extension functions

-- Create a decent randomizer in terms of the Flip randomizer
-- Gets its current line number, the percentage in [0, 100], and the two state parameters
random :: Int -> Float -> Int -> Int -> IO ()
random lineNr p k1 k2 = putStrLn $ rtree p 50 1 -- I NEED MY CURRENT LINE NUMBER!!!
      where
        rtree :: Float -> Float -> Int -> String
        rtree p q d | close p q = compile "Flip" k1 k2
                    | otherwise = if p < q
                                  then (compile "Flip" (lineNr+d) k2) ++ "\n" ++ rtree p (q - getP (d+1)) (d+1)
                                  else (compile "Flip" k1 (lineNr+d)) ++ "\n" ++ rtree p (q + getP (d+1)) (d+1)

        -- Check if the current probability is close enough to the wanted probability
        close :: Float -> Float -> Bool
        close goalP currentP = abs (goalP - currentP) < 1

        -- Get the probability corresponding to the depth of the tree
        getP :: Int -> Float
        getP d = 100 / (2 ** (fromIntegral d))

