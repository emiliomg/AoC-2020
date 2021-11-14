package de.emiliomg.adventofcode.y2020

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import scala.io.Source
import org.scalatest.prop.TableDrivenPropertyChecks

class Day13Test extends AnyFlatSpec with Matchers with TableDrivenPropertyChecks {
  "Day1" should "work with the test input" in {
    Day13.star1(getTestInputStar1) shouldEqual 295
  }

  it should "work with the puzzle input" in {
    val result = Day13.star1(getPuzzleInput)
    println(s"Day13, Star1 $result")
    result shouldEqual 8063
  }

  "Day2" should "work with the test input" in {
    testInputDay2.foreach {
      case (input, expectedResult) =>
        Day13.star2(input) shouldEqual expectedResult
    }
  }

  def getTestInputStar1: String =
    """939
      |7,13,x,x,59,x,31,19""".stripMargin

  val testInputDay2 = Table(
    ("schedule", "expectedTimestamp"),
    ("123\n7,13,x,x,59,x,31,19", 1068781L),
    ("123\n17,x,13,19", 3417L),
    ("123\n67,7,59,61", 754018L),
    ("123\n67,x,7,59,61", 779210L),
    ("123\n67,7,x,59,61", 1261476L),
    ("123\n1789,37,47,1889", 1202161486L)
  )

  def getPuzzleInput: String = Source.fromResource("day13.txt").getLines().mkString("\n")

}
