package org.rosettacode
package pargolfp

import CellularAutomaton.{ CellsAlive, move, nextGeneration }
//import collection.parallel.ParSet

import annotation.tailrec
import language.postfixOps

import org.rosettacode.pargolfp.ConwayPatterns._
import org.scalatest.junit.AssertionsForJUnit
import org.scalatest._

class ModGoL_PatternsSpecTest extends FunSpec with GivenWhenThen {
  import ModGoL_PatternsSpecTest._

  describe("A Conway pattern") {
    it("has periodicity") {
      Given("Still lives")
      Then("each pattern must have a periode of 1.")
      testHarness("S&til Lives", testOscPeriode, "Failure in still lives")

      And("each oscillator has its periodicity")
      testHarness("&Oscillators", testOscPeriode, "Failure in oscillators")

      And("even Spaceships has a periodicity")
      testHarness("&Spaceships", testSpaceshipPeriode, "Failure in spaceships")

      And("single Methuselahs become stable")
      testHarness("&Methuselahs", isStableGeneration, "Failure in methuselahs")
    }
  }
}

object ModGoL_PatternsSpecTest {
  final val WINDOWSIZE = 4 // To check for stable populations, use a window this big  

  // Return an iterator for the generations of a starting pattern
  def conwayIterator(first: CellsAlive) = Iterator.iterate(first)(nextGeneration(_))

  def countExpectedPeriode(first: CellsAlive,
    expectedPeriodeCount: Int,
    isSpaceship: Boolean) = {
    @tailrec
    def inner(pop: CellsAlive, expectedPeriodeCountDown: Int): Boolean = {
      if (expectedPeriodeCountDown <= 0)
        (if (isSpaceship) move(pop, (0, 0)) else pop) == first
      else inner(nextGeneration(pop), expectedPeriodeCountDown - 1)
    }
    inner(first, expectedPeriodeCount)
  }

  /**
   * For each pattern passed, apply a function which will measure some
   *  characteristic of the generations of that pattern, and assert it
   *  is equal to expected value.
   */
  def testHarness(patterns: String,
    test: (CellsAlive, Int) => Boolean,
    msg: String) =
    {
      var testmsg = msg
      assert(patternCollection.get(patterns).get forall {
        case (menuName, pattern, period) => {
          testmsg = f"$menuName%s: $msg%s"
          test(move(pattern, (0, 0)), period)
        }
      }, testmsg)
    }

  def testOscPeriode(first: CellsAlive, expectedPeriodeCount: Int) =
    countExpectedPeriode(first, expectedPeriodeCount: Int, false)

  def testSpaceshipPeriode(first: CellsAlive, expectedPeriodeCount: Int) =
    countExpectedPeriode(first, expectedPeriodeCount: Int, true)

  def isStableGeneration(first: CellsAlive, expectedPeriodeCount: Int) = {
    @tailrec
    def inner(pop: CellsAlive, expectedPeriodeCountDown: Int): Boolean = {
      if (expectedPeriodeCountDown <= 1 || CellularAutomaton.isStablePopulation)
        CellularAutomaton.isStablePopulation
      else inner(nextGeneration(pop), expectedPeriodeCountDown - 1)
    }
    inner(first, expectedPeriodeCount + CellularAutomaton.WINDOWSIZE)
  }

  /**
   * Return the number of generations until the population of a pattern
   * stabilizes. This test only checks a window of generations for
   * population size.
   */
  def getUnstableGenerations(first: CellsAlive, expectedPeriodeCount: Int) = {
    (conwayIterator(first) take (expectedPeriodeCount + WINDOWSIZE) map (_.size) sliding WINDOWSIZE
      map (_.distinct.length) takeWhile (_ > 1) length) == expectedPeriodeCount
  } // def getUnstableGenerations

} // object ModGoL_PatternsSpecTest
