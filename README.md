# YAdoku: Yet another Sudoku solver.

This is a quite simple backtracking Sudoku solver. Easy 9x9 systems are usually solved within some
milliseconds (e.g. example1), very hard ones need few seconds (example3 needs 1.6 seconds with -O3 on an elderly
Core i5 processor).

## `sudoku9` executable

Build the executable file using cabal or `ghc --make -O3 sudoku9.hs`. When executed the program expects a
9x9 sudoku system as first and only argument, e.g.:

    sudoku9 000000500000000010209006003400050000006800002017090000000085007060010309023004000

These digits come from the corresponding system

    X X X  X X X  5 X X
    X X X  X X X  X 1 X
    2 X 9  X X 6  X X 3

    4 X X  X 5 X  X X X
    X X 6  8 X X  X X 2
    X 1 7  X 9 X  X X X

    X X X  X 8 5  X X 7
    X 6 X  X 1 X  3 X 9
    X 2 3  X X 4  X X X

It's solved as

    1 3 4  7 2 8  5 9 6 
    6 7 8  5 3 9  2 1 4
    2 5 9  1 4 6  7 8 3
    
    4 8 2  6 5 3  9 7 1
    5 9 6  8 7 1  4 3 2
    3 1 7  4 9 2  8 6 5
    
    9 4 1  3 8 5  6 2 7
    8 6 5  2 1 7  3 4 9
    7 2 3  9 6 4  1 5 8

## `sudoku9nd` executable

This program shows every possibility how to solve the given initial system.

## Jargon and errors

### Initial integrity

This is the sudoku system from above, except for a small change; the field at (9,9) (*(row,col)*) has been
changed to *9*. This leaves the system in a state lacking *initial integrity*, i.e. it is invalid from begin on.
Such a system makes the parser fail.

    X X X  X X X  5 X X
    X X X  X X X  X 1 X
    2 X 9  X X 6  X X 3

    4 X X  X 5 X  X X X
    X X 6  8 X X  X X 2
    X 1 7  X 9 X  X X X

    X X X  X 8 5  X X 7
    X 6 X  X 1 X  3 X 9
    X 2 3  X X 4  X X 9
