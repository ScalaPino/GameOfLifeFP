package org.rosettacode
package pargolfp

import collection.parallel.ParSet
import language.implicitConversions

/**
 * The object contains a collection of Conway patterns
 *
 * The patterns are stored in string, which can implicitly converted to a Set[Cell]
 */
object ConwayPatterns {

  /**
   * Represents the available Conway patterns in a data-structure
   */
  def patternCollection: Map[String, Seq[(String, String, Int)]] =
    Map("S&til Lives" ->
      Seq(("Beehive", beehive, 1),
        ("Block", block, 1),
        ("Boat", boat, 1),
        ("Loaf", loaf, 1)),
      "&Oscillators" ->
        Seq(("Beacon", beacon, 2),
          ("Blinker", blinker, 2),
          ("Toad", toad, 2),
          ("Pulsar", pulsar, 3),
          ("Eight", eight, 8)),
      "&Spaceships" ->
        Seq(("Glider", glider, 4),
          ("Lightweight spaceships", lwss, 4)),
      "&Methuselahs" ->
        Seq(("&Acorn", acorn, 5206),
          ("&Die hard", diehard, 130),
          ("\"&R\" pentomino", rPentomino, 1103)),
      "&Guns" ->
        Seq(("Gosper gun", gosperGun, 29)),
      ("&Puffers" -> Seq.empty[(String, String, Int)]))

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
  val blinker = """|
                   |
                   | XXX
                   |
                   |"""
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
  implicit def cellsFromPattern(pattern: String) = (for {
    (tupleCharCommaXCharPos, lineNumber) <- pattern.stripMargin.lines.map(_.zipWithIndex).zipWithIndex
    (char, xCharPos) <- tupleCharCommaXCharPos
    if char != ' '
  } yield Cell(xCharPos, lineNumber)).toSet.par

  // Move a set of coordinates to a point
  //  def moveTo(pattern: String, to: Cell) = (pattern: Iterator[Cell]) map (_ + to)
  //def moveTo(coords: Iterator[Cell], to: Cell) = coords map (_ + to)
  //def moveTo(coords: Traversable[Cell], to: Cell) = coords map (_ + to)
} // object ConwayPatterns

