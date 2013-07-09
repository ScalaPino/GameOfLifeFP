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
  PetriDish,
  isStablePopulation,
  moveTo,
  nextGenWithHistory
}
import org.scalatest._

object Main extends FunSpec {
  final val slidingWindow = 4

  def getPeriods(seed: PetriDish): ParSeq[PetriDish] = {
    val reference = moveTo(seed)
    var genCounter = 5206 + slidingWindow
    @tailrec
    def inner(pops: ParSeq[PetriDish]): ParSeq[PetriDish] = {
      val newPops = nextGenWithHistory(pops.par, slidingWindow)
      genCounter -= 1
      assume(genCounter > 0,
        f"Looks like an infinite loop ( >${5206}%d) in getPeriods")
      if (moveTo(newPops.head) == reference ||
        isStablePopulation(newPops, slidingWindow)) newPops
      else inner(newPops /*.seq*/ )
    }
    inner(ParSeq(seed))
  }

  def main(args: Array[String]): Unit = {
    println("Started")

    val b: PetriDish = blinker
    assert(b === ParSet(XYpos(2, 0), XYpos(1, 0), XYpos(3, 0)),
      "String to Generation failed")

    val c = moveTo(b)
    assert(c === ParSet(XYpos(0, 0), XYpos(1, 0), XYpos(-1, 0)), "MoveTo failed")

    assert(boundingBox(c) === (XYpos(-1, 0), XYpos(1, 0)), "Boundingbox failed.")
    println("Generations")
    val blinKer = getPeriods(blinker).reverse
    println("Generation done")
    println(blinKer.map(_.size))
    println(blinKer)
    println("Boundingbox :" + boundingBox(blinKer.flatten.toSet))

    val boundery = boundingBox(blinKer.flatten.toSet)

    for (y <- boundery._2.y to boundery._1.y by -1) {
      for (window <- blinKer.seq)
        for { x <- boundery._1.x to boundery._2.x }
          print(if (window.contains(x, y)) 'X' else ' ')
      println
    }
  } // def main
}