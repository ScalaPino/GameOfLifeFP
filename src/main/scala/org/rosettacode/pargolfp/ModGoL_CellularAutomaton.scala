
/*  _____       _               ____________	*\
** |  __ \     | |              |  ___| ___ \	**
** | |  \/ ___ | |      ______  | |_  | |_/ /	**
** | | __ / _ \| |     |______| |  _| |  __/	**
** | |_\ \ (_) | |____          | |   | |		**
\*  \____/\___/\_____/          \_|   \_|		*/

package org.rosettacode
package pargolfp

import collection.parallel.ParSeq
import annotation.tailrec

/**
 * The basic virtual game. This object contains the functions with living
 * cells. The case of combined XYpos: CellsAlive.
 */
object CellularAutomaton {
  import XYpos.generation

  /**
   * Capable for various rule strings
   */
  def nextGenWithHistory(populations: Generations,
                         WindowSize: Int,
                         rulestringB: Set[Int] = Set(3), // Default to Conway's GoL B3S23
                         rulestringS: Set[Int] = Set(2, 3)): Generations = {

    /**
     * This is the Game of Live engine
     *
     * The next generation is composed of newborns from fecund
     *  neighborhoods and adults on stable neighborhoods.
     */
    def tick(population: PetriDish,
             rulestringB: Set[Int], // Rulestrings describe Life-like rules
             rulestringS: Set[Int]): PetriDish = {
      assume(generation != Int.MaxValue, "Generations outnumbered")
      generation += 1

      /* A Map containing only ''coordinates'' that are neighbors of XYpos which
       * are alive, together with the ''number'' of XYpos it is neighbor of.
       */
      val neighbors =
        (population.toList.flatMap(_.getMooreNeighborhood)).par.groupBy(identity).map {
          case (cell, coll) ⇒ (cell, coll.size)
        }
      // Filter all neighbors for desired characteristics

      // Criterion of rulestring Birth
      def newBorn = neighbors.filter(fFilter ⇒ rulestringB contains fFilter._2).
        keySet
      // Criterion of Survivors rulestring 
      def survivors = population.filter(sieve ⇒ rulestringS contains neighbors.
        getOrElse(sieve, 0))
      return survivors ++ newBorn
    } // def tick(

    // Returning new generation in a collection
    ParSeq(tick(populations.head, rulestringB, rulestringS)) ++
      populations.take(WindowSize - 1)
  } // def nextGenWithHistory(…

  /** Generates 1 */
  def nextGen(populations: PetriDish) =
    nextGenWithHistory(ParSeq(populations), 1).head

  /** Detects a stabilization of the number of living cells */
  def isStablePopulation(pops: Generations, window: Int): Boolean =
    pops.size >= window && pops.tail.forall(_.size == pops.head.size)

  /**
   * Generate a serie of PetriDishes, each is a successor of the previous.
   * Appending is stopped if within the sliding windows the same configuration
   * of living cells reappears. Otherwise it is stopped if the sliding is filled.
   *
   * @param		seed			The initial living cells configuration.
   * @param		slidingWindow	The maximal length of returned
   * @return	The serial sequence of generations in time.
   */
  def getPeriods(seed: PetriDish, slidingWindow: Int): Generations = {
    val reference = moveTo(seed)
    var genCounter = MAX_METHUSELAHS_LIFE + slidingWindow
    @tailrec
    def inner(pops: Generations): Generations = {
      val newPops = nextGenWithHistory(pops.par, slidingWindow)
      genCounter -= 1
      assume(genCounter > 0,
        s"Looks like an infinite loop ( >$MAX_METHUSELAHS_LIFE%d) in getPeriods")
      if (isStablePopulation(newPops, slidingWindow) || // Test if new gen == seed
        moveTo(newPops.head) == reference) newPops
      else inner(newPops)
    }
    inner(ParSeq(seed))
  } // def getPeriods(

  /** Determine the envelope of all cells in a generation*/
  // f: collection.parallel.ParSet[XYpos] => XYpos.Rect
  def boundingBox(gen: PetriDish): XYpos.Rect = {
    if (gen.isEmpty)
      throw new UnsupportedOperationException("empty.boundingBox")
    // Aggregate each XYpos to maximum extreme
    //TODO: Check use of method TrieMap.aggregate
    gen.foldLeft(gen.head extreme gen.head)(
      (resultingRect, currentPos) ⇒ (currentPos extreme resultingRect))
  }

  /**
   * Moves the pattern without altering its disposition
   */
  def moveTo(gen: PetriDish, center: XYpos = (0, 0)): PetriDish = {
    val extremes = boundingBox(gen)
    val offset = XYpos(
      extremes._1.x + (extremes._2.x - extremes._1.x) / 2 - center.x,
      extremes._1.y + (extremes._2.y - extremes._1.y) / 2 - center.y)
    gen.map(_ - offset)
  } // def moveTo

  /** Remove unused XYpos from the cache while keeping given generations.*/
  def flushCache(threshold: Int) {
    val absThreshold = generation - threshold
    if (absThreshold <= generation) { // Prevent underflow
      for (elem ← XYpos.cache.seq)
        if (absThreshold >= elem._2.timestamp) XYpos.cache -= elem._1
    }
  }
} // object CellularAutomaton

//############################################################################