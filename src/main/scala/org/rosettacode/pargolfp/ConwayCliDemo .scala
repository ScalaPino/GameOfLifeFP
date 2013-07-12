/*  _____       _               ____________	*\
** |  __ \     | |              |  ___| ___ \	**
** | |  \/ ___ | |      ______  | |_  | |_/ /	**
** | | __ / _ \| |     |______| |  _| |  __/	**
** | |_\ \ (_) | |____          | |   | |		**
\*  \____/\___/\_____/          \_|   \_|		*/

package org.rosettacode
package pargolfp

import annotation.tailrec
import scala.collection.parallel.ParSeq
import ConwayPatterns._
import XYpos._
import scala.collection.parallel.ParSet
import scala.annotation.tailrec
import CellularAutomaton.{ boundingBox, getPeriods, nextGenWithHistory }

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
      val boundery: Rect = boundingBox(gens.flatten.toSet)
      // Widen by surroundings
      val frame = ((boundery._1.x - 1, boundery._1.y - 1),
        (boundery._2.x + 1, boundery._2.y + 1))
      val frameWidth = frame._2.x - frame._1.x + 1
      var genCounter = -1

      def doGenerations(gens: Generations) {
        def toRow(y: Int) = {
          @tailrec
          def toRowHelper(
            previous: PetriDish,
            actual: Generations,
            ret: Seq[String]): Seq[String] =
            {
              def toChar(point: Tuple2[Int, Int]) = {
                (previous(point), actual.head(point)) match {
                  case (false, false) ⇒ { '\u2219' } // ∙ Empty
                  case (false, true)  ⇒ { '\u263a' } // ☺ Newborn
                  case (true, false)  ⇒ { '\u2022' } // • Just died
                  case (true, true)   ⇒ { '\u263b' } // ☻ Surviver
                }
              } // def toChar(…

              // Begin recursive inner
              if (actual != collection.parallel.ParSeq[PetriDish]())
                toRowHelper(
                  actual.head,
                  actual.tail,
                  ret ++ Seq((if (y == frame._2.y + 1)
                    s"Gen ${genCounter += 1; genCounter}".
                    padTo(frameWidth, " ").mkString // Top line gives gen number
                  else (for (x ← frame._1.x to frame._2.x) // Per dish from left to right X
                    yield toChar((x, y))).mkString)))
              else ret
            }

          // Begin toRow(… 
          toRowHelper(ParSet[XYpos](), gens, Seq()).mkString(" ")
        } // def toRow(…

        // Begin doGenerations(…
        for (y ← frame._2.y + 1 to frame._1.y by -1) println(toRow(y))
      } // def doGenerations(…

      // Begin doPrintlnGenerations(…
      println(s"\nCompute ${friendlyName} generations started")
      doGenerations(gens)
      println(s"${genCounter} gen's $friendlyName")
    } // doPrintlnGenerations(…

    doPrintlnGenerations(blinker, "Blinker pattern")
    doPrintlnGenerations(glider, "Glider pattern")
    doPrintlnGenerations(eight, "Eigth pattern")
  } // def main
}