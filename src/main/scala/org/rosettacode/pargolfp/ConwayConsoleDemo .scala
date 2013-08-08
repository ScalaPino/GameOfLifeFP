/*  _____       _               ____________	*\
** |  __ \     | |              |  ___| ___ \	**
** | |  \/ ___ | |      ______  | |_  | |_/ /	**
** | | __ / _ \| |     |______| |  _| |  __/	**
** | |_\ \ (_) | |____          | |   | |		**
\*  \____/\___/\_____/          \_|   \_|		*/

package org.rosettacode
package pargolfp

import annotation.tailrec
import java.awt.Point

import CellularAutomaton.{ boundingBox, getLimitedLifeSeq }
import collection.parallel.{ ParSeq, ParSet }
import ConwayPatterns._
import XYpos.tupleToXYpos

/** @version		0.4 2013-08-01
 *
 *  @author		Frans W. van den Berg
 *
 *  The "main" object for demonstration purpose.
 *  It get its set of
 */
object ConwayConsoleDemo {
  /** Overrides the global constant */
  final val SLIDINGWINDOW = 9
  final val DummyLong = -1L

  def doPrintGenerations(patternName: String,
                         friendlyName: String = "Pattern",
                         slidingWindow: Int = SLIDINGWINDOW) {

    // Consume the generations
    val gens: GenerationSeq = getLimitedLifeSeq(patternName, slidingWindow)

    // Widen by surroundings
    val frame64: (XYpos, XYpos) = {
      val boundery = boundingBox((gens.map(_._1).flatten.toSet.par, DummyLong))
      (XYpos(boundery._1.x - 1, boundery._1.y - 1),
        XYpos(boundery._2.x + 1, boundery._2.y + 1))
    }
    val frame32 = (new Point(frame64._1.x.toInt, frame64._1.y.toInt),
      new Point(frame64._2.x.toInt, frame64._2.y.toInt))

    def doGenerations(gens: GenerationSeq) {
      def toRow(y: Int): String = {
        @tailrec
        def toRowHelper(previous: PetriDish, actual: GenerationSeq, acc: Seq[String]): Seq[String] =
          {
            def toChar(point: Tuple2[Int, Int]) = {
              (previous._1(point), actual.head._1(point)) match {
                case (false, false) => '∙' //{ '\u2219' } Empty
                case (false, true)  => '☺' //{ '\u263a' } Newborn
                case (true, false)  => '•' //{ '\u2022' } Just died
                case (true, true)   => '☻' //{ '\u263b' } Surviver
              }
            } // def toChar(…

            // Begin recursive toRowHelper
            if (actual != collection.parallel.ParSeq())
              toRowHelper(actual.head,
                actual.tail,
                acc ++ ParSeq((if (y > frame32._2.y) s"Gen ${actual.head._2}".
                  padTo(frame32._2.x - frame32._1.x + 1, " ").mkString // Top line prints gen number
                else (for (x <- frame32._1.x to frame32._2.x) // Per dish from left to right X
                  yield toChar((x, y))).mkString)))
            else acc
          } // def toRowHelper(

        // Begin toRow(… 
        toRowHelper((ParSet(), DummyLong), gens, Seq()).mkString(" ")
      } // def toRow(…

      // Begin doGenerations(…
      if ((frame64._2.x - frame64._1.x) <= 20)
        for (y <- frame32._2.y + 1 to frame32._1.y by -1) println(toRow(y))
      else println("Resulting grid too wide.")
    } // def doGenerations(…

    // Begin doPrintlnGenerations(…
    println(s"\nCompute ${friendlyName} generations started")
    val startTime = System.nanoTime()
    doGenerations(gens)
    val endTime = System.nanoTime()
    val elapsedTime = (endTime - startTime) * 1.e-6

    println(s"${gens.last._2} gen's $friendlyName, stabilized with ${gens.last._1.size} cells." +
      s" Elapsed time = ${elapsedTime} msec")

  } // doPrintlnGenerations(…

  def main(args: Array[String]): Unit = {
    println(s"Legend: ∙ Empty  ☺ New born  ☻ Surviver  • Just died")

    val startTime = System.nanoTime()

    doPrintGenerations(blinker, "Blinker pattern")
    doPrintGenerations(glider, "Glider pattern")
    doPrintGenerations(eight, "Eigth pattern")
    doPrintGenerations(diehard, "Diehard")
    doPrintGenerations(acorn, "Acorn")

    val endTime = System.nanoTime()
    val elapsedTime = (endTime - startTime) * 1.e-6
    println(s"Total elapsed time $elapsedTime msec.")
  } // def main
}