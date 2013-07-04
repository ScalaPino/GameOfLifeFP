package org.rosettacode
package pargolfp

import CellularAutomaton.{
  CellsAlive,
  isStablePopulation,
  moveTo,
  nextGeneration,
  nextGenWithHistory
}
import ConwayPatterns._
import annotation.tailrec
import collection.parallel.ParSet
import language.postfixOps
import org.scalatest.junit.AssertionsForJUnit
import org.scalatest._
import collection.parallel.ParSeq

class ModGoL_PatternsSpecTest extends FunSpec with GivenWhenThen {
  import ModGoL_PatternsSpecTest._

  describe("A Conway pattern") {
    it("has periodicity") {
      Given("Still lives")
      //      Then("each pattern must have a periode of 1.")
      //      testHarness("S&til Lives", testOscPeriode, "Failure in still lives")

      //      Then("each oscillator has its periodicity")
      //      testHarness("&Oscillators", testOscPeriode, "Failure in oscillators")

      //      Then("even Spaceships have periodicity")
      //      testHarness("&Spaceships", countExpectedPeriode, "Failure in spaceships")

      Then("single Methuselahs become stable")
      testHarness("&Methuselahs", isStableGeneration, "Failure in methuselahs")
    }
  }
}

object ModGoL_PatternsSpecTest {

  val WINDOWSIZE = 4

  def countExpectedPeriode(first: CellsAlive, expectedPeriodeCount: Int) = {
    @tailrec
    def inner(pop: CellsAlive,
      expPerCountDown: Int): Boolean = {
      val test = pop == moveTo(pop, (0, 0))
      if ((expPerCountDown <= 0)) {
        test == (expPerCountDown <= 0)
      } else inner(nextGeneration(pop), expPerCountDown - 1)
    }
    inner(first, expectedPeriodeCount)
  }

  /**
   * For each pattern passed, apply a function which will measure some
   *  characteristic of the generations of that pattern, and assert it
   *  is equal to expected value.
   */
  def testHarness(patterns: String,
    test: (CellsAlive, Int, Int) => Boolean,
    msg: String) =
    {
      var testmsg = msg
      assert(patternCollection.get(patterns).get forall {
        case (menuName, pattern, period, popLeft) => {
          testmsg = f"$menuName%s: $msg%s"
          test(moveTo(pattern, (0, 0)), period, popLeft)
        }
      }, testmsg)
    }

  /**
   * Return the number of generations until the population of a pattern
   * stabilizes. This test only checks a window of generations for
   * population size.
   */
  def isStableGeneration(first: CellsAlive,
    expectedPeriodeCount: Int,
    popLeft: Int) = {
    @tailrec
    def inner(pops: ParSeq[CellsAlive], expPerCountDown: Int): Boolean = {
      val newPops = nextGenWithHistory(pops, WINDOWSIZE)
      if (expPerCountDown <= 0 || isStablePopulation(newPops, WINDOWSIZE)) {
        (expPerCountDown <= 0 == isStablePopulation(newPops, WINDOWSIZE)) &&
          newPops.head.size == popLeft
      } else inner(newPops, expPerCountDown - 1)
    }
    inner(ParSeq(first), expectedPeriodeCount + WINDOWSIZE - 2)
  }

  /**
   * Return the number of generations until the population of a pattern
   * stabilizes. This test only checks a window of generations for
   * population size.
   */
  def isStableGeneration0(first: CellsAlive,
    expectedPeriodeCount: Int,
    popLeft: Int) = {
    @tailrec
    def inner(pop: CellsAlive, expPerCountDown: Int): Boolean = {
      val newPop = nextGeneration(pop)
      if (expPerCountDown <= 0 || isStablePopulation) {
        (expPerCountDown <= 0 == isStablePopulation) && newPop.size == popLeft
      } else inner(newPop, expPerCountDown - 1)
    }
    inner(first, expectedPeriodeCount + WINDOWSIZE - 2)
  }

  //  def getUnstableGenerations(first: CellsAlive, expectedPeriodeCount: Int) = {
  //      // Return an iterator for the generations of a starting pattern
  //  def conwayIterator(first: CellsAlive) = Iterator.iterate(first)(nextGeneration(_))
  //
  //    (conwayIterator(first) take (expectedPeriodeCount + WINDOWSIZE) map (_.size) sliding WINDOWSIZE
  //      map (_.distinct.length) takeWhile (_ > 1) length) == expectedPeriodeCount
  //  } // def getUnstableGenerations

} // object ModGoL_PatternsSpecTest
