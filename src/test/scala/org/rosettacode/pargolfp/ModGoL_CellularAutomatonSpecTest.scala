package org.rosettacode
package pargolfp

import org.scalatest._

import ConwayPatterns._

class M_GoL_CellularAutomatonSpecTest
    extends FunSpec
    with GivenWhenThen
    with CellularAutomaton {

  private val (x0, y0) = (3, 25)
  private val (x1, y1) = (x0 + 1, y0)
  private val (x2, y2) = (x1 + 1, y0)

  XYpos.cache.clear

  describe("A Blinker") {
    it("must alternate") {
      Given(f"a horizontal row of 3 lives")
      Then(f"it must be centered ")
      val board: LivingWorld = moveTo(blinker)

      assert(board._1 === Set(XYpos(-1, 0), XYpos(1, 0), XYpos(0, 0)))
     
      And("")
      println("Boaord" + board._1.flatMap(_.getMooreNeighborhood))
      println("Boaord" + tick((board._1,0L)))

      /*      And(f"must be not equal with a neighbor at (${x0 - 1}%d, ${y0 + 1}%d)")
      assert((cell != XYpos(x0 - 1, y0 + 1)))
      And("Exercise addition & substraction")
      cell += XYpos(10, 0)
      expectResult(XYpos(x0 + 10, y0)) { cell }
      cell += XYpos(0, 10)
      expectResult(XYpos(x0 + 10, y0 + 10)) { cell }
      cell -= XYpos(10, 0)
      expectResult(XYpos(x0, y0 + 10)) { cell }
      cell -= XYpos(0, 10)
      expectResult(XYpos(x0, y0)) { cell }
    */ }
    it("generate his neighbor Cells") {
      /*Given(f"a ${XYpos(x0, y0)}%s")
      When("the neigtborhood function is use")
      Then("8 Cells around must be generated without the give Cell")
      expectResult((Vector[XYpos]((x0 - 1, y0 - 1), (x0 - 1, y0), (x0 - 1, y0 + 1),
        (x0, y0 - 1), (x0, y0 + 1),
        (x0 + 1, y0 - 1), (x0 + 1, y0), (x0 + 1, y0 + 1)))) { XYpos(x0, y0).getMooreNeighborhood }
    */ }
  }

  describe("The Cell environment") {

    /*    it("Make a Blinker configuration") {
      while (XYpos.cache.size > 0)
        CellularAutomaton.flushCache(0)

      Given("Cells in a row")
      var universe = ParSet(XYpos(x0, y0), XYpos(x1, y1), XYpos(x2, y2))

      Then("oscilate to crossed position")
      universe = CellularAutomaton.nextGen(universe)
      expectResult(ParSet(XYpos(x1, y1 - 1), XYpos(x1, y1), XYpos(x1, y1 + 1))) { universe }

      And("oscilate back to orginal position")
      universe = CellularAutomaton.nextGen(universe)
      expectResult(ParSet[XYpos]((x0, y0), (x1, y1), (x2, y2))) { universe }

      And(f"Cache has ${XYpos.cache.size}%d cached cells")
      And("Reduce cache  the second deep")
      CellularAutomaton.flushCache(2)
      expectResult(21) { XYpos.cache.size }
      And("Reduce cache  the first level deep")
      CellularAutomaton.flushCache(1)
      expectResult(9) { XYpos.cache.size }
      And("Reduce cache  totally")
      CellularAutomaton.flushCache(0)
      expectResult(0) { XYpos.cache.size }
    }
  */ }
}
