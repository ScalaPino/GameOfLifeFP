package org.rosettacode

/**
 * Conway's Game of Life, a cellular automaton devised in 1970 by the
 * British mathematician John Horton Conway, is considered a zero-player game
 * because its evolution is determined by its initial state, requiring no
 * further input from humans.
 *
 * It is the best-known example of a ''cellular automaton''.
 *
 * A Conway "board" has infinite dimensions -in fact, Conway's Game of Life is
 * Turing complete- so this solution avoids using fixed-size structures
 * such as arrays. Instead, each generation is represented by a __set of the
 * "alive" cells__ on a grid of XY positions.
 * This solution is programmed with best practice and pragmatics
 * of functional programming in Scala with its idiomatics.
 */
package object pargolfp {
  /** Abstract and imaginary habitat for the living cells*/
  type PetriDish = collection.parallel.ParSet[XYpos]
  /** Sequence of consecutive generations*/
  type Generations = collection.parallel.ParSeq[PetriDish]
  /**
   * Limitation of maximal number of iterations. 5206 is the maximum number of
   * generations the Acorn methuselah becomes stable.
   */
  final val MAX_METHUSELAHS_LIFE = 5206
  final val SLIDINGWINDOW = 4
}