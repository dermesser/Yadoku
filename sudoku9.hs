module Main where

import System.Environment
import System.IO
import System.Exit

import Sudoku

main = do
        xs <- getArgs
        sysstring <- if length xs /= 1
                     then putHelp
                     else return . map (\c -> if c == '.' then '0' else c) . head $ xs
        checkErr sysstring
        iosolve $ fromString sysstring
    where checkErr sysstring | length sysstring < order*order     = putStrLn "Supplied system has less than 81 fields! Aborting." >> exitFailure
                             | length sysstring > order*order     = putStrLn "Supplied system has more than 81 fields! Dropping trailing fields, continuing..."
                             | otherwise                          = return ()

putHelp = putStrLn "Usage: $ sudoku <9x9-system>" >> exitFailure
