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

import collection.parallel.{ ParSeq, ParSet }
import ConwayPatterns._

/** @version		0.6 2013-08-01
 *
 *  @author		Frans W. van den Berg
 *
 *  The "main" object for demonstration purpose.
 *  It get its set of
 */
object ConwayConsoleDemo extends CellularAutomaton {
  val slindingWindowSize = 9 // Displays max 8 generations

  private final val DummyLong = -1L

  private def doPrintGenerations(patternName: String,
                                 friendlyName: String = "Pattern") {
    // Compute the limited life generations
    val (startTime: Long, gens, endTime) =
      {
        val cons: LivingWorld = patternName
        (System.nanoTime(),
          getLimitedLifeSeq(cons, slindingWindowSize, cons), // return the subsequent generations
          System.nanoTime())
      }

    // Widen by surroundings
    val frame64: (XYpos, XYpos) = {
      val boundery = boundingBox((gens.map(_._1).flatten.toSet.par, DummyLong))
      (XYpos(boundery._1.x - 1, boundery._1.y - 1),
        XYpos(boundery._2.x + 1, boundery._2.y + 1))
    }
    val frame32 = (new Point(frame64._1.x.toInt, frame64._1.y.toInt),
      new Point(frame64._2.x.toInt, frame64._2.y.toInt))

    /** Print a history of generations to UTF-8 strings
     */
    def generationsToUTF(gens: GenerationSeq): Long = {
      def toRow(y: Int): String = {
        @tailrec
        def toRowHelper(previous: LivingWorld, actual: GenerationSeq, acc: Seq[String]): Seq[String] =
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
                acc ++ ParSeq((if (y > frame32._2.y) f"Gen${actual.head._2}%2d".
                  padTo(frame32._2.x - frame32._1.x + 1, " ").mkString // Top line prints gen number
                else (for (x <- frame32._1.x to frame32._2.x) // Per dish from left to right X
                  yield toChar((x, y))).mkString)))
            else acc
          } // def toRowHelper(

        // Begin toRow(… 
        toRowHelper((ParSet(), DummyLong), gens, Seq()).mkString(" ")
      } // def toRow(…

      // Begin generationsToUTF(…
      if ((frame64._2.x - frame64._1.x) <= 20)
        for (y <- frame32._2.y + 1 to frame32._1.y by -1) println(toRow(y))
      else println("Resulting grid too wide.")
      gens.last._2 // Return # of generations
    } // def generationsToUTF(…

    // Begin doPrintGenerations(…
    println(s"\nCompute ${friendlyName} generations started")

    val nGens = generationsToUTF(gens)
    val elapsedTime = (endTime - startTime) * 1.0e-9
    println(s"$nGens generations $friendlyName, stabilized with ${gens.last._1.size} cells." +
      f" Elapsed time = ${elapsedTime}%.6f sec ~ ${nGens / elapsedTime}%.3f f/s")
  } // doPrintGenerations(…

  def main(args: Array[String]): Unit = {
    println(s"Legend: ∙ Empty  ☺ New born  ☻ Surviver  • Just died")

    val startTime = System.nanoTime()
    doPrintGenerations(blinker, "Blinker pattern")
    doPrintGenerations(glider, "Glider pattern")
    doPrintGenerations(eight, "Eigth pattern")
    doPrintGenerations(diehard, "Diehard")
    doPrintGenerations(acorn, "Acorn")

    println(f"Total elapsed time ${(System.nanoTime() - startTime) * 1.0e-9}%.3f sec.")
  } // def main
}

// ############################################################################