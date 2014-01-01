module Sudoku where

import Data.Matrix
import Data.Word
import Data.List
import Data.Maybe
import qualified Data.Vector as V

-- A sudoku system. We use one-byte integers.
type Sudoku = Matrix Value
type Value = Word8

-- Matrix addressing is in Int.
type Index = Int
type Position = (Index, Index) -- This is (row,column) for compatibility with Data.Matrix

---------------------------
-- Example entry function
---------------------------
iosolve :: Sudoku -> IO ()
iosolve s = do
                let sys = solve s
                case sys of
                    Left s -> putStrLn s
                    Right m -> putStrLn $ prettyMatrix m

---------------------------------------
-- Deterministic (one-result) solver
---------------------------------------

-- Entry; starts solving at left-top corner.
solve :: Sudoku -> Either String Sudoku
solve s = if checkIntegrity s
          then solve' s (1,1)
          else Left "No initial integrity" -- Initial system is not valid.

-- Tries to solve several resulting Sudoku systems for different values at the current position: Backtracking
solve' :: Sudoku -> Position -> Either String Sudoku
solve' s p@(r,c) | r == order && c == order = case sudokuPossibilities of -- This clause is used when we try to solve the last field (position (order,order))
                                                    [] -> Left "No solution was found" -- No solution possible.
                                                    (x:[]) -> Right x
                 | c > order = solve' s (r+1,1)
                 | r > order = solve' s (1,c+1)
                 | otherwise = eitherMap (flip solve' (r+1,c)) sudokuPossibilities -- Search for a successful version of the system
                 where sudokuPossibilities = (map (setField s p) $ possibilities s p)

-- Opposite of mapM: Returns the first result carrying a value. Sufficient for Sudoku backtracking
eitherMap :: (a -> Either String b) -> [a] -> Either String b
eitherMap _ [] = Left "No solution could be found; possibly unsolvable system!" -- This should appear quite rarely. However, there is the possibility of encountering this [1]
eitherMap f (x:xs) = case f x of
                        Right y -> Right y
                        Left e -> eitherMap f xs

{- [1]

Think of this system:

  C    1 2 3 4 5 6 7 8 9

R 1  ( 1 2 3 0 0 0 0 0 0 )
  2  ( 4 5 6 0 0 0 0 0 0 )
  3  ( 7 8 0 9 0 0 0 0 0 )
  4  ( 0 0 0 0 0 0 0 0 0 )
  5  ( 0 0 0 0 0 0 0 0 0 )
  ...

It is not possible to find a solution as there is no
possibility to fill (3,3).

-}

------------------------
-- All-result solver
------------------------

iondsolve :: Sudoku -> IO Int
iondsolve s = do
                let sys = ndsolve s
                case sys of
                    [] -> putStrLn "No acceptable solution was found; maybe there was no inital integrity?" >> return 0
                    xs -> mapM_ print xs >> (return $ length xs)

ndsolve :: Sudoku -> [Sudoku]
ndsolve s = if checkIntegrity s
            then ndsolve' s (1,1)
            else []

ndsolve' :: Sudoku -> Position -> [Sudoku]
ndsolve' s p@(r,c) | r == order && c == order = filter checkIntegrity sudokuPossibilities -- This clause is used when we try to solve the last field (position (order,order))
                 | c > order = ndsolve' s (r+1,1)
                 | r > order = ndsolve' s (1,c+1)
                 | otherwise = listMap (flip ndsolve' (r+1,c)) sudokuPossibilities -- Search for a successful version of the system
                 where sudokuPossibilities = (map (setField s p) $ possibilities s p)


listMap :: (a -> [b]) -> [a] -> [b]
listMap _ [] = []
listMap f (x:xs) = concatMap f (x:xs)

-------------------------
-- No more solver logic
-------------------------


-- Integrity checking --

checkIntegrity :: Sudoku -> Bool
checkIntegrity s = and [ let e = (s ! p); p = (r,c) in -- if the value is not null and appears in row, column or block more than once, something's wrong.
                            e == 0 ||
                            (occs e (block p)) <= 1 &&
                            (occs e (row p)) <= 1 &&
                            (occs e (col p)) <= 1
                       | r <- [1..fromIntegral order], c <- [1..fromIntegral order]]
    where occs = occurrencesInList
          block = blockValues s
          row = rowValues s
          col = colValues s

-- Sudoku Utils --

setField :: Sudoku -> Position -> Value -> Sudoku
setField s p v = setElem v p s

-- Filling possibilities: Which values are currently allowed?
possibilities :: Sudoku -> Position -> [Value]
possibilities s p = case s ! p of
                        0 -> [1..fromIntegral order] \\ (usedValues s p)
                        v -> [v] -- field value if already set

-- This function applies the actual Sudoku constraints.
usedValues :: Sudoku -> Position -> [Value]
usedValues s p = ((blockValues s p) `union` (rowValues s p) `union` (colValues s p)) `intersect` [1..fromIntegral order] -- intersect to remove all 0s

-- Matrix/Index/Position operations

-- Returns the limiting elements. example
{-

The top-left box unit is asked. x marks the elements limiting it ( (1,1) , (4,4) )

x - -  - - - ...
- - -  - - - ...
- - -  - - - ...

- - -  x - - ...
- - -  - - - ...
- - -  - - - ...
...

-}
blockLimits :: Position -> (Position,Position)
blockLimits (r,c) = let c' = ((c-1) `div` suborder)
                        r' = ((r-1) `div` suborder)
                    in ((1 + r'*suborder, 1 + c'*suborder),
                        (1 + (r'+1)*suborder, 1 + (c'+1)*suborder))

-- returns field coordinates of all fields between the top left (r1,c1) and the bottom right (r2,c2) coordinates
blockFields :: (Position,Position) -> [Position]
blockFields ((r1,c1),(r2,c2)) = [ (r,c) | r <- [r1..order], c <- [c1..order], c < c2, r < r2 ]

-- All field values in the unit the field belongs to.
blockValues, rowValues, colValues :: Sudoku -> Position -> [Value]
blockValues s p = map (s!) . blockFields . blockLimits $ p
rowValues s (r,_) = V.toList . getRow r $ s
colValues s (_,c) = V.toList . getCol c $ s

-- Constants

-- order is the side length, suborder the side length of a single box; usually suborder*suborder = order shall hold.
order, suborder :: Int
order = 9
suborder = 3

-- Data

-- Convert a string-represented system to a matrix.
fromString :: String -> Sudoku
fromString = fromList order order . map (read . cons)

-- Input of a Sudoku system as String: type the system line after line and set zeros for empty fields.
{-
example2 represents this system:

( 1 2 3 _ )
( 3 4 _ 2 )
( _ 1 _ 3 )
( 4 _ 2 _ )
-}
example1, example2, example3, example4 :: Sudoku
example1 = fromString "308104590960530001007090430002903718043000059109280600090408005081000904430029107" -- easy one, large
example2 = fromString "1230340201034020" -- kids love it!
example3 = fromString "002500000000020000004000000000050000000200000000400000000000030000000200000009007" -- this is a hard one.
example4 = fromString "100060300020001000003008605000400150400050008058006000905800700000600080001030009"

-- Generic Utils

cons :: a -> [a]
cons = (:[])

occurrencesInList :: Eq a => a -> [a] -> Int
occurrencesInList e = foldl' (\n a -> if a == e then n+1 else n) 0
