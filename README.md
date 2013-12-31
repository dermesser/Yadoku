# YAdoku: Yet another Sudoku solver.

This is a quite simple backtracking Sudoku solver. Easy 9x9 systems are usually solved within some
milliseconds (e.g. example1), very hard ones need few seconds (example3 needs 1.6 seconds with -O3 on an elderly
Core i5 processor).

## `sudoku9` executable

Build the executable file using cabal or `ghc --make -O3 sudoku9.hs`. When executed the program expects a
9x9 sudoku system as first and only argument, e.g.:

    sudoku9 000000500000000010209006003400050000006800002017090000000085007060010309023004000
