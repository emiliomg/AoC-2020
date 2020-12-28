package de.emiliomg.adventofcode.y2020

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import scala.io.Source

class Day8Test extends AnyFlatSpec with Matchers {
  "Star1" should "work with the test data" in {
    Day8.star1(getTestInput) shouldEqual 5
  }

  it should "work with the puzzle data" in {
    val result = Day8.star1(getPuzzleInput)
    println(s"Day8, Star1: $result")
    result shouldEqual 1782
  }

  def getTestInput: String =
    """nop +0
      |acc +1
      |jmp +4
      |acc +3
      |jmp -3
      |acc -99
      |acc +1
      |jmp -4
      |acc +6""".stripMargin

  def getPuzzleInput: String = Source.fromResource("day8.txt").getLines().mkString("\n")
}
