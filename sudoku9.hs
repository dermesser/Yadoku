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
    iosolve $ fromString sysstring

putHelp = putStrLn "Usage: $ sudoku <9x9-system>" >> exitFailure
