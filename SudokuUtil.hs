module SudokuUtil where

import Control.Monad.List
import Data.List

-- Type declarations

type LE e = ListT (Either e)

-- Generic Utils

cons :: a -> [a]
cons = (:[])

occurrencesInList :: Eq a => a -> [a] -> Int
occurrencesInList e = foldl' (\n a -> if a == e then n+1 else n) 0

