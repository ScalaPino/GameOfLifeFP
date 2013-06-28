package org.rosettacode
package pargolfp


import language.{ implicitConversions, postfixOps }

import collection.parallel.mutable.ParHashMap
import collection.parallel.ParSeq
import collection.parallel.ParSet


/**
 *
 * Atomic virtual life cell contains own x,y coordinate and neighbors positions.
 *
 * @version		1.1
 * @param		x Params
 * @param		y Params
 * @param		timestamp Params
 * 
 * @return		Return
 * @since		Since
 * @see			Seee
 * @throws      ssss
 *
 *
 */
class Cell(val x: Int, val y: Int, var timestamp: Int = CellularAutomaton.generation) {

  private lazy val mooreNeighborhoodPos = for {
    dx <- Cell.offsets
    dy <- Cell.offsets
    if (dx != 0 || dy != 0)
  } yield { this + (dx, dy) }

  /**
   * All stored neighbor positions of a cell expressed as a set of Cells.
   * It must be lazy -computed on demand- to postpone evaluation
   * because in this function there are 8 new instances of Cell generated.
   * Direct evaluation should cause a recursive problem.
   */
  def getMooreNeighborhood = {
    timestamp = CellularAutomaton.generation // Update timestamp each time Cell is referred.
    mooreNeighborhoodPos
  }

  // Coordinates can be used as offsets
  private def +(dx: Int, dy: Int) = Cell(x + dx, y + dy)
  /** Coordinates can be used as offsets to add */
  def +(c: Cell): Cell = this + (c.x, c.y)
  /** Coordinates can be used as offsets to subtract */
  def -(c: Cell): Cell = this + (-c.x, -c.y)

  /**
   * override def equals(other: Any):Boolean
   *  There is no need to override equals because the equal comparison is
   *  default by object identity (address). This works great because
   *  for each unique key x,y there is also an unique identity.
   *  This is guaranteed by the Cell's apply method.
   *  There is also no new hash function necessary.
   */

  override def toString = f"Cell($x%d, $y%d)"
} // class Cell

/** This is the companion object for the [[org.rosettacode.pargolfp.Cell]] class.
  *
  * It contains
  * 
  */
object Cell {
  private def offsets = (-1 to 1) // For neighbor selection

  /**
   * The cache will check if the new Cell already exists.
   * If not create a new one with the default method.
   * It makes equality and groupBy (identity) possible.
   */
  val cache: ParHashMap[(Int, Int), Cell] = new ParHashMap[(Int, Int), Cell]() {
    // getOrElseUpdate not available for .par
    override def default(key: (Int, Int)) = {
      val d = new Cell(key._1, key._2); cache((d.x, d.y)) = d /*Update cache */ ; d
    }
  }
  // The Cell factory which checks if the new cell already exists.
  // By means of the default method.
  def apply(x: Int, y: Int): Cell = { cache(x, y) }

  // Any Tuple2[Int, Int] can be used as a Cell through this implicit conversion.  
  implicit def cellFromTuple(t: (Int, Int)) = apply(t._1, t._2)
} // object Cell

/////////////////////////////////////////////////////////////////////////////
// CellularAutomaton section
/**
 * Basic virtual game.
 */
object CellularAutomaton {
  // Some bookkeeping
  final val WINDOWSIZE = 4
  var generation = Int.MinValue
  private var slidingAggregate = ParSeq(0)

  /**
   * The next generation is composed of babies from fecund
   *  neighborhoods and adults on stable neighborhoods.
   */
  def nextGeneration0(population: ParSet[Cell], rulestringS: Set[Int], rulestringB: Set[Int]) :ParSet[Cell] =
    {
      /*
     * A map containing all coordinates that are neighbors of Cells which
     * are alive, together with the number of Cells it is neighbor of.
     */
      assume(generation != Int.MaxValue, "Generations outnumbered")
      generation += 1

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

  def nextGeneration(population: ParSet[Cell],
    rulestringS: Set[Int] = Set(2, 3), // Default Conway's GoL S23B3
    rulestringB: Set[Int] = Set(3)) = {
    val ret = nextGeneration0(population, rulestringS, rulestringB)
    // Registering aggregate data
    slidingAggregate = (if (slidingAggregate.size >= WINDOWSIZE) ParSeq.empty[Int]
    else ParSeq(slidingAggregate.head)) ++ slidingAggregate.tail ++ ParSeq(ret.size)
    ret
  }

  def isStablePopulation = slidingAggregate.tail.forall(_ == slidingAggregate.head)

  // Clear cache history while keeping given generations.
  def flushCache(threshold: Int) {
    val absThreshold = generation - threshold
    if (absThreshold <= generation) { // Prevent underflow
      for (elem <- Cell.cache) yield if ((absThreshold) >= elem._2.timestamp) Cell.cache.remove(elem._1)
    }
  }

  // An Ordering for coordinates which sorts by the X coordinate
  private val xOrdering = Ordering.fromLessThan((_: Cell).x < (_: Cell).x)
  // An Ordering for coordinates which sorts by the Y coordinate
  private val yOrdering = Ordering.fromLessThan((_: Cell).y < (_: Cell).y)

  /**
   * Move the pattern without altering its disposition
   */
  def move(population: ParSet[Cell], center: Cell): ParSet[Cell] = {
    def extremeCells: (Cell, Cell) = {
      (Cell( //This generation's upper left corner Cell
        population min xOrdering x, population max yOrdering y),
        //This generation's lower right corner Cell
        Cell(population max xOrdering x, population min yOrdering y))
    }

    val extremes = extremeCells
    val offset = Cell(
      extremes._1.x + (extremes._2.x - extremes._1.x) / 2 - center.x,
      extremes._2.y + (extremes._1.y - extremes._2.y) / 2 - center.y)
    population map (_ - offset)
  }
} // object CellularAutomaton

