package org.rosettacode
package pargolfp

import CellularAutomaton.{ move, nextGeneration }
import collection.parallel.ParSet

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
  def conwayIterator(first: ParSet[Cell]) = Iterator.iterate(first)(nextGeneration(_))

  def countExpectedPeriode(first: ParSet[Cell],
    expectedPeriodeCount: Int,
    isSpaceship: Boolean) = {
    @tailrec
    def inner(pop: ParSet[Cell], expectedPeriodeCountDown: Int): Boolean = {
      if (expectedPeriodeCountDown <= 0) ((if (isSpaceship) move(pop, (0, 0)) else pop): ParSet[Cell]) == first
      else inner(nextGeneration(pop), expectedPeriodeCountDown - 1)
    }
    inner(first, expectedPeriodeCount)
  }

  /**
   * For each pattern passed, apply a function which will measure some characteristic
   * of the generations of that pattern, and assert it is equal to expected value.
   */
  def testHarness(patterns: String, test: (ParSet[Cell], Int) => Boolean, msg: String) =
    assert(patternCollection.get(patterns).get forall {
      case (menuName, pattern, period) => test(move(pattern, (0, 0)), period)
    }, msg)

  def testOscPeriode(first: ParSet[Cell], expectedPeriodeCount: Int) =
    countExpectedPeriode(first, expectedPeriodeCount: Int, false)

  def testSpaceshipPeriode(first: ParSet[Cell], expectedPeriodeCount: Int) =
    countExpectedPeriode(first, expectedPeriodeCount: Int, true)

  def isStableGeneration(first: ParSet[Cell], expectedPeriodeCount: Int) = {
    @tailrec
    def inner(pop: ParSet[Cell], expectedPeriodeCountDown: Int): Boolean = {
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
  def getUnstableGenerations(first: ParSet[Cell], expectedPeriodeCount: Int) = {
    (conwayIterator(first) take (expectedPeriodeCount + WINDOWSIZE) map (_.size) sliding WINDOWSIZE
      map (_.distinct.length) takeWhile (_ > 1) length) == expectedPeriodeCount
  } // def getUnstableGenerations

} // object ModGoL_PatternsSpecTest
