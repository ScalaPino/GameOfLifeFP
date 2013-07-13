package org.rosettacode
package pargolfp

/**
 *
 * Atomic virtual position contains its own x,y coordinate and neighbors positions.
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
 * @param		x			x part of coordinate pair
 * @param		y			y part of coordinate pair
 * @param		timestamp	Params
 *
 */
class XYpos(val x: Int,
            val y: Int,
            var timestamp: Int = XYpos.generation) {
  import XYpos.{ generation, mooreNeighborhood, Rect }

  /**
   * Private member, see getMooreNeighborhood
   *  Filled with all the 8 XYpos as coordinates around the cell
   */
  private lazy val mooreNeighborhoodPos =
    mooreNeighborhood.map(p ⇒ this plus (p._1, p._2)) // Avoid implicit conversion

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
   * Resulting point is could be distant of both points!
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

  // Init the static variables
  private def offsets = (-1 to 1).seq // For neighbor selection
  private val mooreNeighborhood = (for {
    dx ← offsets; dy ← offsets; if dx != 0 || dy != 0
  } yield (dx, dy))

  /**
   * The cache will check if the new XYpos already exists.
   * If not create a new one with the default method.
   * It makes equality and groupBy (identity) possible.
   * Least Recently Used policy can be used on this cache.
   */
  val cache = collection.concurrent.TrieMap((0, 0) -> new XYpos(0, 0))

  /**
   * The XYpos factory which checks if the new XYpos already exists.
   * By means of the default method of the HashMap
   */
  def apply(x: Int, y: Int): XYpos =
    { cache.getOrElseUpdate((x, y), new XYpos(x, y)) }

  /**A Tuple2[Int, Int] can be used as a XYpos through this implicit conversion.*/
  implicit def tupleToXYpos(tup: (Int, Int)): XYpos = apply(tup._1, tup._2)
} // object XYpos
