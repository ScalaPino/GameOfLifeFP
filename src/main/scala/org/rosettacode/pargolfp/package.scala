/*  _____       _               ____________	*\
** |  __ \     | |              |  ___| ___ \	**
** | |  \/ ___ | |      ______  | |_  | |_/ /	**
** | | __ / _ \| |     |______| |  _| |  __/	**
** | |_\ \ (_) | |____          | |   | |		**
\*  \____/\___/\_____/          \_|   \_|		*/

package org.rosettacode

/** Conway's Game of Life, a cellular automaton devised in 1970 by the
 *  British mathematician John Horton Conway, is considered a zero-player game
 *  because its evolution is only determined by its initial state, requiring no
 *  further input from humans.
 *
 *  It is the best-known example of a ''cellular automaton''.
 *
 *  A Conway "board" has infinite dimensions -in fact, Conway's Game of Life is
 *  Turing complete- so this solution avoids using fixed-size structures
 *  such as arrays. Instead, each generation is represented by a __set of the
 *  "alive" cells__ on a grid of XY positions.
 *  This solution is programmed with best practice and pragmatics
 *  of functional programming in Scala with its idiomatics.
 */
package object pargolfp {
  // Rectangular area of cells is XYpos constellation
  type Rect = (XYpos, XYpos)

  /** Abstract and imaginary habitat for the living cells*/
  type LivingWorld = (collection.parallel.ParSet[XYpos], Long /* Generation counter*/ )

  /** Sequence of consecutive generations*/
  type GenerationSeq = collection.parallel.ParSeq[LivingWorld]
}

// ############################################################################