package de.emiliomg.adventofcode.y2020

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import scala.io.Source

class Day13Test extends AnyFlatSpec with Matchers {
  "Day1" should "work with the test input" in {
    Day13.star1(getTestInput) shouldEqual 295
  }

  it should "work with the puzzle input" in {
    val result = Day13.star1(getPuzzleInput)
    println(s"Day13, Star1 $result")
    result shouldEqual 8063
  }

  def getTestInput: String =
    """939
      |7,13,x,x,59,x,31,19""".stripMargin

  def getPuzzleInput: String = Source.fromResource("day13.txt").getLines().mkString("\n")

}
