GameOfLifeFP
============

Conway's Game of Life - Functional Programmed in Scala 

A scala implementation of Conway's Game of Life, a cellular automaton devised in 1970 by the British mathematician John Horton Conway. It is considered a zero-player or solitaire game because its evolution is determined by its initial state, requiring no further input from humans.

It is the best-known example of a cellular automaton and probably the most often programmed computer game in existence.

A Conway grid is infinite two-dimensional orthogonal. In fact, Conway's Game of Life is Turing complete, so this implementation avoids using fixed-size structures such as arrays. Instead, each generation is represented by a __set of the "alive" cells__. So the presence of a coordinate in a set will signal the fact of a living cell.

This solution is programmed with best practice and pragmatics of functional programming in Scala with its idiomatics.
