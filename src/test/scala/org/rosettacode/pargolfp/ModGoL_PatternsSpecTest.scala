package org.rosettacode
package pargolfp

import CellularAutomaton.{
  Generation,
  isStablePopulation,
  moveTo,
  nextGenWithHistory
}
import ConwayPatterns._

import annotation.tailrec
import collection.parallel.{ParSeq, ParSet}
import language.postfixOps
import org.scalatest._

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
      testHarness("&Methuselahs", testLifeSpan, "Failure in methuselahs")
    }
  }
}

object ModGoL_PatternsSpecTest {

  //  def countExpectedPeriode(first: CellsAlive, expectedPeriodeCount: Int) = {
  //    @tailrec
  //    def inner(pop: CellsAlive,
  //      expPerCountDown: Int): Boolean = {
  //      val test = pop == moveTo(pop, (0, 0))
  //      if ((expPerCountDown <= 0)) {
  //        test == (expPerCountDown <= 0)
  //      } else inner(nextGeneration(pop), expPerCountDown - 1)
  //    }
  //    inner(first, expectedPeriodeCount)
  //  }

  /**
   * For each pattern passed, apply a function which will measure some
   *  characteristic of the generations of that pattern, and assert it
   *  is equal to expected value.
   */
  def testHarness(patterns: String,
    test: (Generation, Int, Int) => Boolean,
    msg: String) =
    {
      var testmsg = msg
      assert(patternCollection.get(patterns).get forall {
        case (menuName, pattern, period, popLeft) => {
          testmsg = f"$menuName%s: $msg%s"
          test(moveTo(pattern), period, popLeft)
        }
      }, testmsg)
    }

  /**
   * Return the number of generations until the population of a pattern
   * stabilizes. This test only checks a window of generations for
   * population size.
   */
  def testLifeSpan(seed: Generation,
    expectedPeriodeCount: Int,
    expectedPopLeft: Int) = {
    val slidingWindowSize = 4
    @tailrec
    def inner(pops: ParSeq[Generation], expPerCountDown: Int): Boolean = {
      val newPops = nextGenWithHistory(pops, slidingWindowSize)
      // The termination condition, either lifespan count
      // or no change in the number of living cells.
      if (expPerCountDown <= 0 || isStablePopulation(newPops, slidingWindowSize)) {
        // If termination condition valid, return if the reason is valid.
        (expPerCountDown <= 0 == isStablePopulation(newPops, slidingWindowSize)) &&
          newPops.head.size == expectedPopLeft // Stable population count
      } else inner(newPops, expPerCountDown - 1) // Else continue
    }
    inner(ParSeq(seed), expectedPeriodeCount + slidingWindowSize - 2)
  }

  //  def getUnstableGenerations(first: CellsAlive, expectedPeriodeCount: Int) = {
  //      // Return an iterator for the generations of a starting pattern
  //  def conwayIterator(first: CellsAlive) = Iterator.iterate(first)(nextGeneration(_))
  //
  //    (conwayIterator(first) take (expectedPeriodeCount + WINDOWSIZE) map (_.size) sliding WINDOWSIZE
  //      map (_.distinct.length) takeWhile (_ > 1) length) == expectedPeriodeCount
  //  } // def getUnstableGenerations

} // object ModGoL_PatternsSpecTest
