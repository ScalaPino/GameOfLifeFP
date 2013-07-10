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

object Main {
  final val slidingWindow = 9

  def getPeriods(seed: PetriDish): ParSeq[PetriDish] = {
    val reference = moveTo(seed)
    var genCounter = 5206 + slidingWindow
    @tailrec
    def inner(pops: ParSeq[PetriDish]): ParSeq[PetriDish] = {
      val newPops = nextGenWithHistory(pops.par, slidingWindow)
      genCounter -= 1
      assume(genCounter > 0,
        f"Looks like an infinite loop ( >${5206}%d) in getPeriods")
      if (isStablePopulation(newPops, slidingWindow)||
          moveTo(newPops.head) == reference
        ) newPops
      else inner(newPops /*.seq*/ )
    }
    inner(ParSeq(seed))
  }

  def main(args: Array[String]): Unit = {

    def doPrintlnGenerations(patternName: String,
      friendlyName: String = "Pattern") {
      println(s"Compute ${friendlyName} generations started")

      val gens = getPeriods(patternName).reverse

      val boundery = boundingBox(gens.flatten.toSet)
      val frameWidth = boundery._2.x - boundery._1.x + 2
      println(frameWidth)

      var genCounter = -1
      for (y <- boundery._2.y + 1 to boundery._1.y by -1) {

        println(
          (for { window <- gens.seq }
            yield if (y == boundery._2.y + 1) { f"Gen ${genCounter += 1; genCounter}%d" }
          else (for (x <- boundery._1.x to boundery._2.x)
            yield if (window.contains(x, y)) '☻' else '·').mkString).mkString(" "))
      }
      println(s"${genCounter} gen's $friendlyName")
    }

    doPrintlnGenerations(blinker, "Blinker")
    doPrintlnGenerations(glider, "Eight")

  } // def main
}