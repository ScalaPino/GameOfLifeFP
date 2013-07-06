package org.rosettacode
package pargolfp

//import collection.parallel.ParSet
import language.implicitConversions

/**
 * The object contains a collection of Conway patterns
 *
 * The patterns are stored in string, which can implicitly converted to a Set[XYpos]
 */
object ConwayPatterns {

  /**
   * Represents the available Conway patterns in a data-structure
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
                 |"""
  /** Still Live pattern: Beehive */
  val beehive = """|
                   |  XX
                   | X  X
                   |  XX
                   |"""
  /** Still Live pattern: Loaf */
  val loaf = """|
                |  XX
                | X  X
                |  X X
                |   X
                |"""
  /** Still Live pattern: Boat */
  val boat = """|
                | XX
                | X X
                |  X
                |"""

  /** Oscillator pattern: Blinker */
  val blinker = """| XXX"""
  /** Oscillator pattern: Toad */
  val toad = """|
                |
                |  XXX
                | XXX
                |
                |"""
  /** Oscillator pattern: Beacon */
  val beacon = """|
                  | XX
                  | XX
                  |   XX
                  |   XX
                  |"""
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
                  |"""
  /** Oscillator pattern: Eight */
  val eight = """|
                 |XXX
                 |XXX
                 |XXX
                 |   XXX
                 |   XXX
                 |   XXX
                 |"""

  /** Spaceship pattern: Glider */
  val glider = """|
                  |   X
                  | X X
                  |  XX
                  |"""
  /** Spaceship pattern: Lightweight Spaceship */
  val lwss = """|
                |
                |  XXXX
                | X   X
                |     X
                | X  X
                |"""
  /** Spaceship pattern: block */

  /** Methuselah pattern: Die hard */
  val diehard = """|
                   |       X
                   | XX
                   |  X   XXX
                   |"""

  /** Methuselah pattern: Acorn */
  val acorn = """|
                 |  X
                 |    X
                 | XX  XXX
                 |"""

  /** Methuselah pattern: "R" pentomino */
  val rPentomino = """|
                      | XX
                      |  XX
                      |  X
                      |"""

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
                     |"""

  // Helper methods
  // Enable constructing sets of coordinates from string patterns.
  /**
   *
   */
  implicit def generationFromPattern(pattern: String) = (for {
    (tupleCharCommaXCharPos, lineNumber) <- pattern.
      stripMargin.lines.map(_.zipWithIndex).zipWithIndex
    (char, xCharPos) <- tupleCharCommaXCharPos
    if char != ' '
  } yield XYpos(xCharPos, lineNumber)).toSet.par
} // object ConwayPatterns

