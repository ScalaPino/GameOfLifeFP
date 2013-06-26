package org.rosettacode
package pargolfp

import org.scalatest._
import org.scalatest.junit.AssertionsForJUnit
import collection.parallel.ParSet

class M_GoL_CellularAutomatonSpecTest extends FunSpec with GivenWhenThen {
  val (x0, y0) = (3, 25)
  val (x1, y1) = (x0 + 1, y0)
  val (x2, y2) = (x1 + 1, y0)

  describe("A Cell") {
    it("has is equality function") {
      Given(f"a Cell($x0%d, $y0%d)")
      var cell = Cell(x0, y0)

      Then(f"the must equal with ${Cell(x0, y0)}%s")
      assert(cell === Cell(x0, y0))

      And(f"must be not equal with a neighbor at (${x0 - 1}%d, ${y0 + 1}%d)")
      assert((cell != Cell(x0 - 1, y0 + 1)))
      And("Exercise addition & substraction")
      cell += Cell(10, 0)
      expectResult(Cell(x0 + 10, y0)) { cell }
      cell += Cell(0, 10)
      expectResult(Cell(x0 + 10, y0 + 10)) { cell }
      cell -= Cell(10, 0)
      expectResult(Cell(x0, y0 + 10)) { cell }
      cell -= Cell(0, 10)
      expectResult(Cell(x0, y0)) { cell }
    }
    it("generate his neighbor Cells") {
      Given(f"a ${Cell(x0, y0)}%s")
      When("the neigtborhood function is use")
      Then("8 Cells around must be generated without the give Cell")
      expectResult((Vector[Cell]((x0 - 1, y0 - 1), (x0 - 1, y0), (x0 - 1, y0 + 1),
        (x0, y0 - 1), (x0, y0 + 1),
        (x0 + 1, y0 - 1), (x0 + 1, y0), (x0 + 1, y0 + 1)))) { Cell(x0, y0).getMooreNeighborhood }
    }
  }

  describe("The Cell environment") {

    it("Make a Blinker configuration") {
      while (Cell.cache.size > 0)
        CellularAutomaton.flushCache(0)

      Given("Cells in a row")
      var universe = ParSet(Cell(x0, y0), Cell(x1, y1), Cell(x2, y2))

      Then("oscilate to crossed position")
      universe = CellularAutomaton.nextGeneration(universe)
      expectResult(ParSet(Cell(x1, y1 - 1), Cell(x1, y1), Cell(x1, y1 + 1))) { universe }

      And("oscilate back to orginal position")
      universe = CellularAutomaton.nextGeneration(universe)
      expectResult(ParSet[Cell]((x0, y0), (x1, y1), (x2, y2))) { universe }

      And(f"Cache has ${Cell.cache.size}%d cached cells")
      Then("Reduce cache  the second deep")
      CellularAutomaton.flushCache(2)
      expectResult(21) { Cell.cache.size }
      And("Reduce cache  the first level deep")
      CellularAutomaton.flushCache(1)
      expectResult(9) { Cell.cache.size }
      And("Reduce cache  totally")
      CellularAutomaton.flushCache(0)
      expectResult(0) { Cell.cache.size }
    }
  }
}