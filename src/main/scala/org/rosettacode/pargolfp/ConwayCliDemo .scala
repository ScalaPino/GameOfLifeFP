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
  getPeriods,
  isStablePopulation,
  moveTo,
  nextGenWithHistory
}

/**
 * The "main" object
 */
object ConwayCliDemo {
  /** Overrides the global constant */
  final val SLIDINGWINDOW = 9

  def main(args: Array[String]): Unit = {

    def doPrintlnGenerations(patternName: String,
      friendlyName: String = "Pattern",
      slidingWindow: Int = SLIDINGWINDOW) {

      val gens: Generations = getPeriods(patternName, slidingWindow).reverse
      val boundery = boundingBox(gens.flatten.toSet)
      val frame = ((boundery._1.x - 1, boundery._1.y - 1),
        (boundery._2.x + 1, boundery._2.y + 1))
      val frameWidth = frame._2.x - frame._1.x + 1
      var genCounter = -1

      def doMatrixis(gens: Generations) {
        def toRow(y: Int) = {
          def toChar(window: PetriDish, point: Tuple2[Int, Int]) = {
            if (window.contains(point)) '☻' else '·'
          } // def toChar(…

          // Begin toRow(…
          (for { window: PetriDish ← gens.seq } yield // Do first PetriDish to last
          if (y == frame._2.y + 1) s"Gen ${genCounter += 1; genCounter}".
            padTo(frameWidth, " ").mkString // Top line gives gen number
          else (for (x ← frame._1.x to frame._2.x) // Per dish from left to right X
            yield toChar(window, (x, y))).mkString).mkString(" ")
        } // def toRow(…

        // Begin toMatrixis(…
        for (y ← frame._2.y + 1 to frame._1.y by -1) println(toRow(y))
      } // def toMatrixis(…

      // Begin doPrintlnGenerations(…
      println(s"\nCompute ${friendlyName} generations started")
      doMatrixis(gens)
      println(s"${genCounter} gen's $friendlyName")
    } // doPrintlnGenerations(…

    doPrintlnGenerations(blinker, "Blinker pattern")
    doPrintlnGenerations(glider, "Glider pattern")
    doPrintlnGenerations(eight, "Eigth pattern")
  } // def main
}