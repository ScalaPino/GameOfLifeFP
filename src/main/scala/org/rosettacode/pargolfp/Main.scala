/*  _____       _               ____________	*\
** |  __ \     | |              |  ___| ___ \	**
** | |  \/ ___ | |      ______  | |_  | |_/ /	**
** | | __ / _ \| |     |______| |  _| |  __/	**
** | |_\ \ (_) | |____          | |   | |		**
\*  \____/\___/\_____/          \_|   \_|		*/

package org.rosettacode
package pargolfp

import scala.collection.parallel.ParSeq
import ConwayPatterns._
import XYpos._
import scala.collection.parallel.ParSet
import scala.annotation.tailrec
import CellularAutomaton.{ boundingBox, Generation, moveTo, nextGenWithHistory }
import org.scalatest._

object Main extends FunSpec {

  def getPeriods(seed: Generation): ParSeq[Generation] = {
    @tailrec
    def inner(pops: ParSeq[Generation]): ParSeq[Generation] = {
      val newPops = nextGenWithHistory(pops, 4)
      if (moveTo(newPops.head) == seed) newPops else inner(newPops)
    }
    inner(ParSeq(seed))
  }

  def main(args: Array[String]): Unit = {

    def ourMap(gen: Generation, fn: XYpos => XYpos.Rect) = {
      gen.foldRight(List[XYpos.Rect]()) {
        (x: XYpos, xs: List[XYpos.Rect]) => fn(x) :: xs
      }
    }

    def doIt(n: XYpos): XYpos.Rect = {
      boundingBox(ParSet((0,0)))
    }

    def doBounding(numbers: ParSet[XYpos]): XYpos.Rect =
      numbers.foldLeft(tupleToXYpos(0,0),tupleToXYpos(0,0)) {
        (resultingTuple, currentInteger) =>
          (doIt(currentInteger))
      }

    def listWithSum(numbers: List[Int]) =
      numbers.foldLeft((List[Int](), 0)) {
        (resultingTuple, currentInteger) =>
          (currentInteger :: resultingTuple._1, currentInteger + resultingTuple._2)
      }

    val b = generationFromPattern(blinker)
    assert(b === ParSet(XYpos(2, 0), XYpos(1, 0), XYpos(3, 0)), "String to Generation")
    
    println(boundingBox(b))
    
    val c = moveTo(b)
    assert(c === ParSet(XYpos(0, 0), XYpos(1, 0), XYpos(-1, 0)), "MoveTo failed")

    val d = boundingBox(c)
    assert(d === (XYpos(-1, 0), XYpos(1, 0)), "Boundingbox failed.")

    val blinKer = getPeriods(moveTo(b))

    println(blinKer)

    println("Boundingbox :" + boundingBox(blinKer.flatten.toSet.par))

  }

}