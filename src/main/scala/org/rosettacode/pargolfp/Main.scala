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
import CellularAutomaton.{
  boundingBox,
  Generation,
  isStablePopulation,
  moveTo,
  nextGenWithHistory
}
import org.scalatest._

object Main extends FunSpec {
  final val slidingWindow = 4

  def getPeriods(seed: Generation): ParSeq[Generation] = {
    val reference = moveTo(seed)
    var genCounter = 5206 + slidingWindow
    @tailrec
    def inner(pops: ParSeq[Generation]): ParSeq[Generation] = {
      val newPops = nextGenWithHistory(pops.par, slidingWindow)
      genCounter -= 1
      assume(genCounter > 0,
        f"Looks like an infinite loop ( >${5206}%d) in getPeriods")
      if (moveTo(newPops.head) == reference ||
        isStablePopulation(newPops, slidingWindow)) newPops
      else inner(newPops/*.seq*/)
    }
    inner(ParSeq(seed))
  }

  def main(args: Array[String]): Unit = {
    println("Started")
    def ourMap(gen: Generation, fn: XYpos => XYpos.Rect) = {
      gen.foldRight(List[XYpos.Rect]()) {
        (x: XYpos, xs: List[XYpos.Rect]) => fn(x) :: xs
      }
    }

    val b = generationFromPattern(blinker)
    assert(b === ParSet(XYpos(2, 0), XYpos(1, 0), XYpos(3, 0)), "String to Generation failed")

    val c = moveTo(b)
    assert(c === ParSet(XYpos(0, 0), XYpos(1, 0), XYpos(-1, 0)), "MoveTo failed")

    assert(boundingBox(c) === (XYpos(-1, 0), XYpos(1, 0)), "Boundingbox failed.")
    println("Generations")
    val blinKer = getPeriods(generationFromPattern(acorn))
    println("Generation done")
    println(blinKer.map(_.size))

    println("Boundingbox :" + boundingBox(blinKer.flatten.toSet))

  }

}