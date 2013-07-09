package org.rosettacode
package pargolfp

import language.{ implicitConversions, postfixOps }
import collection.parallel.ParSeq
import collection.concurrent.TrieMap

/**
 *
 * Atomic virtual position contains own x,y coordinate and neighbors positions.
 * If included in the Lives set its "alive".
 *
 * @version		0.1 2013-07-01
 *
 * @author		Frans W. van den Berg
 *
 * @note		There is no need to override equals because the equal comparison is
 *  default by object identity (address). This works great because
 *  for each unique key x,y there is also an unique identity.
 *  This is guaranteed by the XYpos's apply method.
 *  There is also no new hash function necessary.
 *
 * @constructor	Create a new Thing instance from a Wotsit. Upon creating x,y
 * 				coordinates will be checked for uniqueness. See [[XYpos$]]
 *
 * @param		x x part of coordinate pair
 * @param		y y part of coordinate pair
 * @param		timestamp Params
 *
 */
class XYpos(val x: Int,
  val y: Int,
  var timestamp: Int = XYpos.generation) {
  import XYpos.{ generation, mooreNeighborhood, Rect }

  private lazy val mooreNeighborhoodPos =
    mooreNeighborhood.map(p => this plus (p._1, p._2)) // Avoid implicit conversion
  /**
   * All stored neighbor positions of a cell expressed as a set of XYpos.
   * It must be lazy -computed on demand- to postpone evaluation
   * because in this function there are 8 new instances of XYpos generated.
   * Direct evaluation should cause a recursive problem.
   */
  def getMooreNeighborhood = {
    timestamp = generation // Update timestamp each time XYpos is referred.
    mooreNeighborhoodPos
  }

  // XYpos can be used as an offset
  private def plus(dx: Int, dy: Int) = XYpos(x + dx, y + dy)

  /** Add a XYpos as an offset */
  def +(that: XYpos): XYpos = this plus (that.x, that.y)

  /** Subtract a XYpos as an offset */
  def -(that: XYpos): XYpos = this plus (-that.x, -that.y)

  /**
   * Get the maximal respectively minimal numbers of either coordinate.
   * Resulting point is mostly distant of both points!
   */
  private def max(that: XYpos) = XYpos(this.x max that.x, this.y max that.y)
  private def min(that: XYpos) = XYpos(this.x min that.x, this.y min that.y)

  /**
   * Get a ''rectangular'' that envelopes both __points__ with
   *  a bottom-left and an upper-left hand coordinate.
   */
  def extreme(that: XYpos): Rect = ((this min that), (this max that))

  /**
   * Get a ''rectangular'' with a bottom-left and an upper-left hand
   * coordinate that envelopes this point and a __rectangle__.
   */
  def extreme(that: Rect): Rect =
    (((this min that._1) min that._2), (this max that._1) max that._2)

  /** Output the class textual */
  override def toString = (f"XYpos($x%d, $y%d)")
} // class XYpos

/**
 * This is the companion object for the [[XYpos]] class.
 *
 * It contains
 *
 */
object XYpos {
  type Rect = (XYpos, XYpos)

  var generation = Int.MinValue

  private def offsets = (-1 to 1) // For neighbor selection
  private val mooreNeighborhood = (for {
    dx <- offsets.seq
    dy <- offsets.seq
    if dx != 0 || dy != 0
  } yield (dx, dy))

  /**
   * The cache will check if the new XYpos already exists.
   * If not create a new one with the default method.
   * It makes equality and groupBy (identity) possible.
   * Least Recently Used policy can be used on this cache.
   */
  val cache = TrieMap((0, 0) -> new XYpos(0, 0))

  /**
   * The XYpos factory which checks if the new XYpos already exists.
   * By means of the default method of the HashMap
   */
  def apply(x: Int, y: Int): XYpos =
    { cache.getOrElseUpdate((x, y), new XYpos(x, y)) }

  /**A Tuple2[Int, Int] can be used as a XYpos through this implicit conversion.*/
  implicit def tupleToXYpos(t: (Int, Int)): XYpos = apply(t._1, t._2)
} // object XYpos

/////////////////////////////////////////////////////////////////////////////
// CellularAutomaton section
/**
 * The basic virtual game. This object contains the functions with living
 * cells. The case of combined XYpos: CellsAlive.
 */
object CellularAutomaton {
  import XYpos.generation
  type Generation = collection.parallel.ParSet[XYpos]

  /**
   *
   */
  def nextGenWithHistory(populations: ParSeq[Generation],
    WindowSize: Int,
    rulestringB: Set[Int] = Set(3), // Default to Conway's GoL B3S23
    rulestringS: Set[Int] = Set(2, 3)): ParSeq[Generation] = {
    /**
     * The next generation is composed of newborns from fecund
     *  neighborhoods and adults on stable neighborhoods.
     */
    def nextGeneration(population: Generation,
      rulestringB: Set[Int], // Rulestrings describe Life-like rules
      rulestringS: Set[Int]): Generation = {
      assume(generation != Int.MaxValue, "Generations outnumbered")
      generation += 1

      /* A map containing all coordinates that are neighbors of XYpos which
       * are alive, together with the number of XYpos it is neighbor of.
       */
      val neighbors =
        (population.toList flatMap (_.getMooreNeighborhood)).par groupBy (identity) map {
          case (cell, coll) => (cell, coll.size)
        }
      // Filter all neighbors for desired characteristics

      // Criterion of rulestring Birth
      def reproductions = neighbors.filter(fFilter => rulestringB contains fFilter._2).keys
      // Criterion of Survivors rulestring 
      def survivors = neighbors.filter(fFilter => /*test n XYpos then AND previous existence */
        (rulestringS contains fFilter._2) && (population contains fFilter._1)).keySet
      (survivors ++ reproductions)
    } // def nextGeneration

    // Returning new generation in a collection
    ParSeq(nextGeneration(populations.head, rulestringB, rulestringS)) ++
      populations.take(WindowSize - 1)
  } // def nextGenWithHistory(...

  /** Detects a stabilization of the number of living cells */
  def isStablePopulation(pops: ParSeq[Generation], window: Int): Boolean =
    pops.size >= window && pops.tail.forall(_.size == pops.head.size)

  /** Determine the envelope of all cells in a generation*/
  // f: collection.parallel.ParSet[XYpos] => XYpos.Rect
  def boundingBox(gen: Generation): XYpos.Rect = {
    if (gen.isEmpty)
      throw new UnsupportedOperationException("empty.boundingBox")
    gen.foldLeft(gen.head extreme gen.head) {
      (resultingRect, currentPos) => (currentPos extreme resultingRect)
    }
  }

  /**
   * Moves the pattern without altering its disposition
   */
  def moveTo(gen: Generation, center: XYpos = (0, 0)): Generation = {
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
      for (elem <- XYpos.cache )
        if (absThreshold >= elem._2.timestamp) XYpos.cache -= elem._1
    }
  }
} // object CellularAutomaton

//############################################################################