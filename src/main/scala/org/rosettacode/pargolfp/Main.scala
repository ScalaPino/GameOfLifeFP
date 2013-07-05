/*                  _____       _               ____________                  *\
**                 |  __ \     | |              |  ___| ___ \                 **
**                 | |  \/ ___ | |      ______  | |_  | |_/ /                 **
**                 | | __ / _ \| |     |______| |  _| |  __/                  **
**                 | |_\ \ (_) | |____          | |   | |                     **
\*                  \____/\___/\_____/          \_|   \_|                     */  


package org.rosettacode.pargolfp

import scala.collection.parallel.ParSeq
import ConwayPatterns._
import XYpos._
import scala.collection.parallel.ParSet
import scala.annotation.tailrec
import CellularAutomaton.{ boundingBox, Generation, moveTo, nextGenWithHistory }

object Main {

  def getPeriods(seed: Generation): ParSeq[Generation] = {
    @tailrec
    def inner(pops: ParSeq[Generation]): ParSeq[Generation] = {
      val newPops = nextGenWithHistory(pops, 4)
      if (moveTo(newPops.head) == seed) newPops else inner(newPops)
    }
    inner(ParSeq(seed))
  } 

  def main(args: Array[String]): Unit = {
    
    val blinKer = getPeriods(moveTo(blinker))

    println("Flatten: " + blinKer.flatten)

    println(boundingBox(blinKer.flatten.toSet.par))

  }

}