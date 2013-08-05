/*  _____       _               ____________	*\
** |  __ \     | |              |  ___| ___ \	**
** | |  \/ ___ | |      ______  | |_  | |_/ /	**
** | | __ / _ \| |     |______| |  _| |  __/	**
** | |_\ \ (_) | |____          | |   | |		**
\*  \____/\___/\_____/          \_|   \_|		*/

package org.rosettacode
package pargolfp

import annotation.tailrec
import collection.parallel.ParSeq

import CellularAutomaton.{ boundingBox, getLimitedLifeSeq }
import ConwayPatterns._
import XYpos.tupleToXYpos

/** @version		0.3 2013-08-01
 *
 *  @author		Frans W. van den Berg
 *
 *  The "main" object
 */
object ConwayConsoleDemo {
  /** Overrides the global constant */
  final val SLIDINGWINDOW = 9

  def doPrintGenerations(patternName: String,
                         friendlyName: String = "Pattern",
                         slidingWindow: Int = SLIDINGWINDOW) {
    println(s"Sliding window = $slidingWindow")

    // Consume the generations
    val gens: GenerationSeq =
      getLimitedLifeSeq(patternName, slidingWindow).par.reverse

    // Widen by surroundings
    val frame = {
      val boundery = boundingBox(gens.flatten.toSet.par);
      ((boundery._1.x - 1, boundery._1.y - 1),
        (boundery._2.x + 1, boundery._2.y + 1))
    }
    var genCounter = -1

    def doGenerations(gens: GenerationSeq) {
      def toRow(y: Int): String = {
        @tailrec
        def toRowHelper(previous: PetriDish, actual: GenerationSeq, acc: Seq[String]): Seq[String] =
          {
            def toChar(point: Tuple2[Int, Int]) = {
              (previous(point), actual.head(point)) match {
                case (false, false) ⇒ { '\u2219' } // ∙ Empty
                case (false, true)  ⇒ { '\u263a' } // ☺ Newborn
                case (true, false)  ⇒ { '\u2022' } // • Just died
                case (true, true)   ⇒ { '\u263b' } // ☻ Surviver
              }
            } // def toChar(…

            // Begin recursive toRowHelper
            if (actual != collection.parallel.ParSeq())
              toRowHelper(actual.head, actual.tail,
                acc ++ ParSeq((if (y > frame._2.y) s"Gen ${genCounter += 1; genCounter}".
                  padTo(frame._2.x - frame._1.x + 1, " ").mkString // Top line prints gen number
                else (for (x ← frame._1.x to frame._2.x) // Per dish from left to right X
                  yield toChar((x, y))).mkString)))
            else acc
          } // def toRowHelper(

        // Begin toRow(… 
        toRowHelper(collection.parallel.ParSet(), gens, Seq()).mkString(" ")
      } // def toRow(…

      // Begin doGenerations(…
      for (y ← frame._2.y + 1 to frame._1.y by -1) println(toRow(y))
    } // def doGenerations(…

    // Begin doPrintlnGenerations(…
    println(s"\nCompute ${friendlyName} generations started")
    doGenerations(gens)
    println(s"${genCounter} gen's $friendlyName")
  } // doPrintlnGenerations(…

  def main(args: Array[String]): Unit = {
    doPrintGenerations(blinker, "Blinker pattern")
    doPrintGenerations(glider, "Glider pattern")
    //doPrintlnGenerations(eight, "Eigth pattern")
  } // def main
}