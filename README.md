# YAdoku: Yet another Sudoku solver.

This is a quite simple backtracking Sudoku solver. It's fast for most systems intended for humans
(every tested sudoku from this category needed less than a second, usually less than 0.25 seconds), but can
be very slow on very hard ones (like Peter Norvig's [top95](http://norvig.com/top95.txt)).

## `sudoku9` executable

Build the executable file using cabal or `ghc --make -O3 sudoku9.hs`. When executed the program expects a
9x9 sudoku system as first and only argument, e.g.:

    sudoku9 000000500000000010209006003400050000006800002017090000000085007060010309023004000

These digits represent the corresponding system

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

Starting with

    1 0 0  0 6 0  3 0 0
    0 2 0  0 0 1  0 0 0
    0 0 3  0 0 8  6 0 5

    0 0 0  4 0 0  1 5 0
    4 0 0  0 5 0  0 0 8
    0 5 8  0 0 6  0 0 0

    9 0 5  8 0 0  7 0 0
    0 0 0  6 0 0  0 8 0
    0 0 1  0 3 0  0 0 2

(`100060300020001000003008605000400150400050008058006000905800700000600080001030002`)

`sudoku9nd` will find two possible solutions:

    1 8 4  9 6 5  3 2 7
    5 2 6  3 7 1  8 9 4
    7 9 3  2 4 8  6 1 5

    6 7 2  4 8 9  1 5 3
    4 1 9  7 5 3  2 6 8
    3 5 8  1 2 6  4 7 9

    9 4 5  8 1 2  7 3 6
    2 3 7  6 9 4  5 8 1
    8 6 1  5 3 7  9 4 2


    1 8 4  9 6 5  3 2 7
    5 2 6  3 7 1  8 4 9
    7 9 3  2 4 8  6 1 5

    6 7 2  4 8 9  1 5 3
    4 1 9  7 5 3  2 6 8
    3 5 8  1 2 6  9 7 4

    9 4 5  8 1 2  7 3 6
    2 3 7  6 9 4  5 8 1
    8 6 1  5 3 7  4 9 2


    ===
    2 solutions were found.

## Jargon and errors

### Initial integrity

This is the sudoku system from the example described in the `sudoku9` section, except for
a small change; the field at (9,9) (*(row,col)*) has been changed to *9*. This leaves the
system in a state lacking *initial integrity*, i.e. it is invalid from begin on.
The solver will reject this system before even starting to solve (which saves you precious
CPU time in the case of a typo).

    X X X  X X X  5 X X
    X X X  X X X  X 1 X
    2 X 9  X X 6  X X 3

    4 X X  X 5 X  X X X
    X X 6  8 X X  X X 2
    X 1 7  X 9 X  X X X

    X X X  X 8 5  X X 7
    X 6 X  X 1 X  3 X 9
    X 2 3  X X 4  X X 9

Another error which is caught by the solver before trying to solve a system is an unsolvable configuration like this:

    1 2 3  0 0 0  0 0 0
    4 5 6  0 0 0  0 0 0
    7 8 0  9 0 0  0 0 0

    0 0 0  0 0 0  0 0 0
    0 0 0  0 0 0  0 0 0
    0 0 0  0 0 0  0 0 0

    0 0 0  0 0 0  0 0 0
    0 0 0  0 0 0  0 0 0
    0 0 0  0 0 0  0 0 0

There is no possible value to be filled in `(3,3)`.

