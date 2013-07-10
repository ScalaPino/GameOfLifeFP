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
  isStablePopulation,
  moveTo,
  nextGenWithHistory
}

object GoL_FP {
  final val SLIDINGWINDOW = 9

  def getPeriods(seed: PetriDish, slidingWindow: Int) = {
    val reference = moveTo(seed)
    var genCounter = MAX_METHUSELAHS_LIFE + slidingWindow
    @tailrec
    def inner(pops: ParSeq[PetriDish]): ParSeq[PetriDish] = {
      val newPops = nextGenWithHistory(pops.par, slidingWindow)
      genCounter -= 1
      assume(genCounter > 0,
        s"Looks like an infinite loop ( >$MAX_METHUSELAHS_LIFE%d) in getPeriods")
      if (isStablePopulation(newPops, slidingWindow) ||
        moveTo(newPops.head) == reference) newPops
      else inner(newPops)
    }
    inner(ParSeq(seed))
  }

  def main(args: Array[String]): Unit = {

    def doPrintlnGenerations(patternName: String,
      friendlyName: String = "Pattern",
      slidingWindow: Int = SLIDINGWINDOW) {

      println(s"\nCompute ${friendlyName} generations started")

      val gens = getPeriods(patternName, slidingWindow).reverse
      val boundery = boundingBox(gens.flatten.toSet)
      val frame = ((boundery._1.x - 1, boundery._1.y - 1),
        (boundery._2.x + 1, boundery._2.y + 1))
      val frameWidth = frame._2.x - frame._1.x + 1
      var genCounter = -1

      for (y ← frame._2.y + 1 to frame._1.y by -1) { // Top to bottom Y
        println((for { window ← gens.seq } // Do first PetriDish to last
          yield if (y == frame._2.y + 1) s"Gen ${genCounter += 1; genCounter}".
          padTo(frameWidth, " ").mkString // Top line gives gen number
        else (for (x ← frame._1.x to frame._2.x) // Per dish from left to right X
          yield if (window.contains(x, y)) '☻' else '·').mkString).
          mkString(" ")) // Intercolumn separator
      }
      println(s"${genCounter} gen's $friendlyName")
    } // doPrintlnGenerations(…

    doPrintlnGenerations(blinker, "Blinker pattern")
    doPrintlnGenerations(glider, "Glider pattern")
    doPrintlnGenerations(eight, "Eigth pattern")
  } // def main
}