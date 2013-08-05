
/*  _____       _               ____________	*\
** |  __ \     | |              |  ___| ___ \	**
** | |  \/ ___ | |      ______  | |_  | |_/ /	**
** | | __ / _ \| |     |______| |  _| |  __/	**
** | |_\ \ (_) | |____          | |   | |		**
\*  \____/\___/\_____/          \_|   \_|		*/

package org.rosettacode
package pargolfp

import annotation.tailrec

/** The basic virtual game. This object contains the functions with living
 *  cells. The case of combined XYpos: CellsAlive.
 *
 *  @version		0.3 2013-08-01
 *
 *  @author		Frans W. van den Berg
 */
object CellularAutomaton {
  import XYpos.generation

  /** Detects a stabilization of the number of living cells */
  def isStablePopulation(pops: GenerationSeq, window: Int): Boolean =
    pops.size >= 2 * window && pops.slice(window, 2 * window).forall(_.size == pops.head.size)

  /** This is the Game of Live engine
   *
   *  The next generation is composed of newborns from fecund
   *  neighborhoods and adults on stable neighborhoods.
   */
  def tick(population: PetriDish,
           rulestringB: Set[Int] = Set(3), // Default to Conway's GoL B3S23
           rulestringS: Set[Int] = Set(2, 3)): PetriDish = {
    assume(generation != Int.MaxValue, "Generations outnumbered")
    generation += 1

    /** A Map containing only ''coordinates'' that are neighbors of XYpos which
     *  are alive, together with the ''number'' of XYpos it is neighbor of.
     */
    val neighbors =
      (population.toList.flatMap(_.getMooreNeighborhood)).par.groupBy(identity).map {
        case (cell, coll) => (cell, coll.size)
      }
    // Filter all neighbors for desired characteristics

    // Criterion of rulestring Birth
    def newBorn = neighbors.filter(fFilter => rulestringB contains fFilter._2).
      keySet
    // Criterion of Survivors rulestring 
    def survivors = population.filter(sieve => rulestringS contains neighbors.
      getOrElse(sieve, 0))
    return survivors ++ newBorn
  } // def tick(…

  def dummy(dish: GenerationSeq, a: Int, b: Int) = false

  /** Generate a stream of PetriDishes, each is a successor of the previous.
   *  Appending is stopped if within the sliding windows the same configuration
   *  of living cells reappears.
   *
   *  @param		seed			The initial living cells configuration.
   *  @param		slidingWindow	The maximal length of returned
   *  @return	The serial sequence of generations in time.
   */
  def getLimitedLifeSeq(seed: PetriDish,
                        WindowSize: Int,
                        callback: (GenerationSeq, Int, Int) => Boolean = dummy): GenerationSeq =
    {
      val reference = moveTo(seed)
      @tailrec
      def inner(pops: GenerationSeq): GenerationSeq = {
        val nextGen = tick(pops.head) +: pops.take(2 * WindowSize - 1)
        // Add last generation in the stream and check for end condition.
        if (pops.isEmpty ||
          moveTo(nextGen.head) == reference ||
          callback(nextGen, 0, 0)) nextGen
        else if (isStablePopulation(nextGen, WindowSize)) nextGen.drop(nextGen.length - WindowSize)
        else inner(nextGen)
      }
      // Begin of getLifeStream
      inner(collection.parallel.ParSeq(seed))
    } // def getLifeStream(…

  /** Determine the envelope of all cells in a generation*/
  def boundingBox(gen: PetriDish): Rect = {
    if (gen.isEmpty)
      throw new UnsupportedOperationException("empty.boundingBox")
    // Aggregate each XYpos to maximum extreme
    //TODO: Check use of method TrieMap.aggregate
    gen.foldLeft(gen.head extreme gen.head)(
      (resultingRect, currentPos) ⇒ (currentPos extreme resultingRect))
  }

  /** Moves the pattern without altering its disposition
   */
  def moveTo(gen: PetriDish, center: XYpos = (0, 0)): PetriDish = {
    val extremes = boundingBox(gen)
    val offset = XYpos(
      extremes._1.x + (extremes._2.x - extremes._1.x) / 2 - center.x,
      extremes._1.y + (extremes._2.y - extremes._1.y) / 2 - center.y)
    gen.map(_ - offset)
  } // def moveTo(…

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