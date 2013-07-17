/*  _____       _               ____________	*\
** |  __ \     | |              |  ___| ___ \	**
** | |  \/ ___ | |      ______  | |_  | |_/ /	**
** | | __ / _ \| |     |______| |  _| |  __/	**
** | |_\ \ (_) | |____          | |   | |		**
\*  \____/\___/\_____/          \_|   \_|		*/

package org.rosettacode
package pargolfp

import annotation.tailrec
import collection.parallel.{ ParSet, ParSeq }

import CellularAutomaton.{ boundingBox, getLifeStream }
import ConwayPatterns._
import XYpos.tupleToXYpos

/**
 * The "main" object
 */
object ConwayCliDemo {
  /** Overrides the global constant */
  final val SLIDINGWINDOW = 9

  def doPrintlnGenerations(patternName: String,
                           friendlyName: String = "Pattern",
                           slidingWindow: Int = SLIDINGWINDOW) {
   // Consume the generations from a stream into 
    val gens: GenerationSeq = 
      getLifeStream(patternName).take(slidingWindow).toSeq.par
    val boundery = boundingBox(gens.flatten.toSet.par)
    // Widen by surroundings
    val frame = ((boundery._1.x - 1, boundery._1.y - 1), (boundery._2.x + 1, boundery._2.y + 1))
    var genCounter = -1

    def doGenerations(gens: GenerationSeq) {
      def toRow(y: Int): String = {
        @tailrec
        def toRowHelper(previous: PetriDish, actual: GenerationSeq, ret: Seq[String]): Seq[String] =
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
            if (actual != collection.parallel.ParSeq())
              toRowHelper(actual.head, actual.tail, ret ++
                ParSeq((if (y > frame._2.y) s"Gen ${genCounter += 1; genCounter}".
                  padTo(frame._2.x - frame._1.x + 1, " ").mkString // Top line gives gen number
                else (for (x ← frame._1.x to frame._2.x) // Per dish from left to right X
                  yield toChar((x, y))).mkString)))
            else ret
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
    doPrintlnGenerations(blinker, "Blinker pattern")
    doPrintlnGenerations(glider, "Glider pattern")
    doPrintlnGenerations(eight, "Eigth pattern")
  } // def main
}