package org.rosettacode
package pargolfp

import language.{ implicitConversions, postfixOps }
import collection.parallel.mutable.ParHashMap
import collection.parallel.ParSeq

/**
 *
 * Atomic virtual position contains own x,y coordinate and neighbors positions.
 * If included in the Lives set its "alive".
 *
 * @version		0.1 2013-07-01
 *
 * @author		Frans W. van den Berg
 *
 * @constructor	Create a new Thing instance from a Wotsit. Upon creating x,y
 * 				coordinates will be checked for uniqueness. See [[XYpos$]]
 *
 * @param		x x part of coordinate pair
 * @param		y y part of coordinate pair
 * @param		timestamp Params
 */
class XYpos(val x: Int,
  val y: Int,
  var timestamp: Int = XYpos.generation) {
  import XYpos._
  /**
   * override def equals(other: Any):Boolean
   *  There is no need to override equals because the equal comparison is
   *  default by object identity (address). This works great because
   *  for each unique key x,y there is also an unique identity.
   *  This is guaranteed by the Cell's apply method.
   *  There is also no new hash function necessary.
   */

  private lazy val mooreNeighborhoodPos = mooreNeighborhood.map(p => this + p)

  /**
   * All stored neighbor positions of a cell expressed as a set of Cells.
   * It must be lazy -computed on demand- to postpone evaluation
   * because in this function there are 8 new instances of Cell generated.
   * Direct evaluation should cause a recursive problem.
   */
  def getMooreNeighborhood = {
    timestamp = XYpos.generation // Update timestamp each time Cell is referred.
    mooreNeighborhoodPos
  }

  // XYpos can be used as an offset
  private def +(dx: Int, dy: Int) = XYpos(x + dx, y + dy)

  /** Add a XYpos as an offset */
  def +(that: XYpos): XYpos = this + (that.x, that.y)

  /** Subtract a XYpos as an offset */
  def -(that: XYpos): XYpos = this + (-that.x, -that.y)

  /**
   * Get the maximal numbers of either coordinate.
   * Resulting point is mostly distant of both points!
   */
  def max(that: XYpos) = XYpos(this.x max that.x, this.y max that.y)

  /**
   * Get the minimal numbers of either coordinate.
   * Resulting point is mostly distant of both points!
   */
  def min(that: XYpos) = XYpos(this.x min that.x, this.y min that.y)

  // Get a rectangle is describing with the extreme positions
  /**
   * Get a rectangular that envelopes both points with
   *  a bottom-left and an upper-left hand coordinate.
   */
  def extreme(that: XYpos): XYpos.Rect = ((this min that), (this max that))

  def extreme(that: XYpos.Rect): XYpos.Rect =
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
  private val mooreNeighborhood = for {
    dx <- XYpos.offsets
    dy <- XYpos.offsets
    if dx != 0 || dy != 0
  } yield (dx, dy)

  /**
   * The cache will check if the new Cell already exists.
   * If not create a new one with the default method.
   * It makes equality and groupBy (identity) possible.
   * Least Recently Used policy can be used on this cache.
   */
  val cache: ParHashMap[(Int, Int), XYpos] =
    new ParHashMap[(Int, Int), XYpos]() {
      override def default(key: (Int, Int)) = { // Key not found
        val newPos = new XYpos(key._1, key._2)
        cache((key._1, key._2)) = newPos /*Update cache */
        newPos
      }
    }
  // The Cell factory which checks if the new XYpos already exists.
  // By means of the default method.
  def apply(x: Int, y: Int): XYpos = { cache(x, y) }

  /** A Tuple2[Int, Int] can be used as a XYpos through this implicit conversion.*/
  implicit def tupleToXYpos(t: (Int, Int)): XYpos = apply(t._1, t._2)
} // object XYpos

/////////////////////////////////////////////////////////////////////////////
// CellularAutomaton section
/**
 * Basic virtual game.
 */
object CellularAutomaton {
  import XYpos.generation
  type CellsAlive = collection.parallel.ParSet[XYpos]

  // Some bookkeeping
  final val WINDOWSIZE = 4
  private var slidingAggregate = ParSeq(0)

  /**
   * The next generation is composed of newborns from fecund
   *  neighborhoods and adults on stable neighborhoods.
   */
  def nextGeneration0(population: CellsAlive,
    // Rulestrings describe Life-like rules
    rulestringS: Set[Int],
    rulestringB: Set[Int]): CellsAlive =
    {
      assume(generation != Int.MaxValue, "Generations outnumbered")
      generation += 1

      /*
     * A map containing all coordinates that are neighbors of Cells which
     * are alive, together with the number of Cells it is neighbor of.
     */
      val neighbors =
        (population.toList flatMap (_.getMooreNeighborhood)).par groupBy (identity) map {
          case (cell, list) => (cell, list.length)
        }
      // Filter all neighbors for desired characteristics

      // Criterion of rulestring Birth
      def reproductions = neighbors.filter(fFilter => rulestringB contains fFilter._2).keys
      // Criterion of Survivors rulestring 
      def survivors = neighbors.filter(fFilter => /*test n Cells then AND previous existence */
        (rulestringS contains fFilter._2) && (population contains fFilter._1)).keySet
      (survivors ++ reproductions)
    } // def nextGeneration0

  def nextGeneration(population: CellsAlive,
    rulestringS: Set[Int] = Set(2, 3), // Default Conway's GoL S23B3
    rulestringB: Set[Int] = Set(3)) = {
    val ret = nextGeneration0(population, rulestringS, rulestringB)
    // Registering aggregate data
    slidingAggregate = (if (slidingAggregate.size >= WINDOWSIZE) ParSeq.empty[Int]
    else ParSeq(slidingAggregate.head)) ++ slidingAggregate.tail ++ ParSeq(ret.size)
    ret
  }

  // Remove unused XYpos from the cache while keeping given generations.
  def flushCache(threshold: Int) {
    val absThreshold = generation - threshold
    if (absThreshold <= generation) { // Prevent underflow
      for (elem <- XYpos.cache)
        if ((absThreshold) >= elem._2.timestamp) XYpos.cache.remove(elem._1)
    }
  }

  /** Detects a stabilization of the number of living cells */
  def isStablePopulation = slidingAggregate.tail.forall(_ == slidingAggregate.head)

  /**
   * Move the pattern without altering its disposition
   */
  def move(population: CellsAlive, center: XYpos): CellsAlive = {

    def extremeCellsAlive: XYpos.Rect = {
      if (population.isEmpty)
        throw new UnsupportedOperationException("empty.CellsAlive")
      var first = true
      var acc: XYpos.Rect = ((0, 0), (0, 0)) //0.asInstanceOf[XYpos.Rect]
      for (x <- population) {
        if (first) {
          acc = x extreme x
          first = false
        } else acc = x extreme acc
      }
      acc
    }

    val extremes = extremeCellsAlive
    val offset = XYpos(
      extremes._1.x + (extremes._2.x - extremes._1.x) / 2 - center.x,
      extremes._1.y + (extremes._2.y - extremes._1.y) / 2 - center.y)
    population map (_ - offset)
  }
} // object CellularAutomaton

//############################################################################