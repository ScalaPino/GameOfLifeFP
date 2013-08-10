/*  _____       _               ____________	*\
** |  __ \     | |              |  ___| ___ \	**
** | |  \/ ___ | |      ______  | |_  | |_/ /	**
** | | __ / _ \| |     |______| |  _| |  __/	**
** | |_\ \ (_) | |____          | |   | |		**
\*  \____/\___/\_____/          \_|   \_|		*/

package org.rosettacode
package pargolfp

import language.implicitConversions
/** The object contains a collection of Conway patterns
 *
 *  The patterns are stored in string, which can implicitly converted to a Set[XYpos]
 */
object ConwayPatterns {

  /** Represents the available Conway patterns in a data-structure
   */
  def patternCollection =
    Map("S&til Lives" ->
      Seq(("Beehive", beehive, 1, 0),
        ("Block", block, 1, 0),
        ("Boat", boat, 1, 0),
        ("Loaf", loaf, 1, 0)),
      "&Oscillators" ->
        Seq(("Beacon", beacon, 2, 0),
          ("Blinker", blinker, 2, 0),
          ("Toad", toad, 2, 0),
          ("Pulsar", pulsar, 3, 0),
          ("Eight", eight, 8, 0)),
      "&Spaceships" ->
        Seq(("Glider", glider, 4, 0),
          ("Lightweight spaceships", lwss, 4, 0)),
      "&Methuselahs" ->
        Seq(("&Acorn", acorn, 5206, 633),
          ("&Die hard", diehard, 130, 0),
          ("\"&R\" pentomino", rPentomino, 1103, 116)),
      "&Guns" ->
        Seq(("Gosper gun", gosperGun, 29, 0)),
      ("&Puffers" -> Seq.empty[(String, String, Int, Int)]))

  /** Still Live pattern: Block */
  val block = """|
                 | XX
                 | XX
                 |""".stripMargin
  /** Still Live pattern: Beehive */
  val beehive = """|
                   |  XX
                   | X  X
                   |  XX
                   |""".stripMargin
  /** Still Live pattern: Loaf */
  val loaf = """|
                |  XX
                | X  X
                |  X X
                |   X
                |""".stripMargin
  /** Still Live pattern: Boat */
  val boat = """|
                | XX
                | X X
                |  X
                |""".stripMargin

  /** Oscillator pattern: Blinker */
  val blinker = """| XXX""".stripMargin
  /** Oscillator pattern: Toad */
  val toad = """|
                |
                |  XXX
                | XXX
                |
                |""".stripMargin
  /** Oscillator pattern: Beacon */
  val beacon = """|
                  | XX
                  | XX
                  |   XX
                  |   XX
                  |""".stripMargin
  /** Oscillator pattern: Pulsar */
  val pulsar = """|
                  |
                  |    XXX   XXX
                  |
                  |  X    X X    X
                  |  X    X X    X
                  |  X    X X    X
                  |    XXX   XXX
                  |
                  |    XXX   XXX
                  |  X    X X    X
                  |  X    X X    X
                  |  X    X X    X
                  |
                  |    XXX   XXX
                  |
                  |""".stripMargin
  /** Oscillator pattern: Eight */
  val eight = """|
                 |XXX
                 |XXX
                 |XXX
                 |   XXX
                 |   XXX
                 |   XXX
                 |""".stripMargin

  /** Spaceship pattern: Glider */
  val glider = """|
                  |   X
                  | X X
                  |  XX
                  |""".stripMargin
  /** Spaceship pattern: Lightweight Spaceship */
  val lwss = """|
                |
                |  XXXX
                | X   X
                |     X
                | X  X
                |""".stripMargin
  /** Spaceship pattern: block */

  /** Methuselah pattern: Die hard */
  val diehard = """|
                   |       X
                   | XX
                   |  X   XXX
                   |""".stripMargin

  /** Methuselah pattern: Acorn */
  val acorn = """|
                 |  X
                 |    X
                 | XX  XXX
                 |""".stripMargin

  /** Methuselah pattern: "R" pentomino */
  val rPentomino = """|
                      | XX
                      |  XX
                      |  X
                      |""".stripMargin

  /** Guns pattern: Gosper Gun */
  val gosperGun = """|
                     |                        X
                     |                      X X
                     |            XX      XX            XX
                     |           X   X    XX            XX
                     |XX        X     X   XX
                     |XX        X   X XX    X X
                     |          X     X       X
                     |           X   X
                     |            XX
                     |""".stripMargin

  // Helper methods
  // Enable constructing sets of coordinates from string patterns.
  /**
   */
  implicit def generationFromPattern(pattern: String): PetriDish = ((for {
    (tupleCharCommaXCharPos, lineNumber) <- pattern.stripMargin.lines.map(_.zipWithIndex).zipWithIndex
    (char, xCharPos) <- tupleCharCommaXCharPos if char != ' '
  } yield XYpos(xCharPos, -lineNumber)).toSet.par, 0L)
} // object ConwayPatterns

// ############################################################################

