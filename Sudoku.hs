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
iosolve = putStrLn . fromMaybe "No solution found" . fmap prettyMatrix . solve

-- Entry; starts solving at left-top corner.
solve :: Sudoku -> Maybe Sudoku
solve s = solve' s (1,1)

-- Tries to solve several resulting Sudoku systems for different values at the current position: Backtracking
solve' :: Sudoku -> Position -> Maybe Sudoku
solve' s p@(r,c) | r == order && c == order = case sudokuPossibilities of -- This clause is used when we try to solve the last field (position (order,order))
                                                    [] -> Nothing -- No solution possible.
                                                    (x:[]) -> Just x -- Hopefully only one element!
                 | c > order = solve' s (r+1,1)
                 | r > order = solve' s (1,c+1)
                 | otherwise = maybeMap (flip solve' (r+1,c)) sudokuPossibilities -- Search for a successful version of the system
                 where sudokuPossibilities = (map (setField s p) $ possibilities s p)

-- Opposite of mapM: Returns the first result carrying a value. Sufficient for Sudoku backtracking
maybeMap :: (a -> Maybe b) -> [a] -> Maybe b
maybeMap _ [] = Nothing
maybeMap f (x:xs) = case f x of
                        Just y -> Just y
                        Nothing -> maybeMap f xs

-------------------------
-- No more solver logic
-------------------------


-- Integrity checking

checkIntegrity :: Sudoku -> Bool
checkIntegrity s = and [ (s ! (r,c)) `elem` (0 : theoreticalPossibilities s (r,c)) | r <- [1..fromIntegral order], c <- [1..fromIntegral order]]

-- Sudoku Utils --

setField :: Sudoku -> Position -> Value -> Sudoku
setField s p v = setElem v p s

-- Filling possibilities: Which values are currently allowed?
possibilities :: Sudoku -> Position -> [Value]
possibilities s p = case s ! p of
                        0 -> [1..fromIntegral order] \\ (usedValues s p)
                        v -> [v]

theoreticalPossibilities :: Sudoku -> Position -> [Value]
theoreticalPossibilities s p = [1..fromIntegral order] \\ ((usedValues s p) \\ [s ! p])

-- This function applies the Sudoku constraints.
usedValues :: Sudoku -> Position -> [Value]
usedValues s p@(r,c) = ((blockValues s p) `union` (rowValues s r) `union` (colValues s c)) `intersect` [1..fromIntegral order] -- intersect to remove all 0s

-- Matrix/Index/Position operations

blockLimits :: Position -> (Position,Position)
blockLimits (r,c) = let c' = ((c-1) `div` suborder)
                        r' = ((r-1) `div` suborder)
                    in ((1 + r'*suborder, 1 + c'*suborder),
                        (1 + (r'+1)*suborder, 1 + (c'+1)*suborder))

blockPositions :: (Position,Position) -> [Position]
blockPositions ((r1,c1),(r2,c2)) = [ (r,c) | r <- [r1..order], c <- [c1..order], c < c2, r < r2 ]

blockValues :: Sudoku -> Position -> [Value]
blockValues s p = map (s!) . blockPositions . blockLimits $ p

rowValues, colValues :: Sudoku -> Index -> [Value]
rowValues s i = V.toList . getRow i $ s
colValues s i = V.toList . getCol i $ s

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
example1 = fromString $ "308104590960530001007090430002903718043000059109280600090408005081000904430029107" -- easy one, large
example2 = fromString $ "1230340201034020" -- kids love it!
example3 = fromString $ "002500000000020000004000000000050000000200000000400000000000030000000200000009007" -- this is a hard one.
example4 = fromString $ "100060300020001000003008605000400150400050008058006000905800700000600080001030009"

-- Generic Utils

cons :: a -> [a]
cons = (:[])
