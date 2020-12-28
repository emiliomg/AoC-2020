package de.emiliomg.adventofcode.y2020

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import scala.io.Source

class Day9Test extends AnyFlatSpec with Matchers {
  "Star1" should "work with the test data" in {
    Day9.star1(getTestInput, 5) shouldEqual 127
  }

  it should "work with the puzzle data" in {
    val result = Day9.star1(getPuzzleInput, 25)
    println(s"Day9, Star1: $result")
    result shouldEqual 257342611
  }

  "Star2" should "work with the test data" in {
    Day9.star2(getTestInput, 5) shouldEqual 62
  }

  it should "work with the puzzle input" in {
    val result = Day9.star2(getPuzzleInput, 25)
    println(s"Day9, Star2: $result")
    result shouldEqual 35602097
  }

  def getTestInput: String =
    """35
      |20
      |15
      |25
      |47
      |40
      |62
      |55
      |65
      |95
      |102
      |117
      |150
      |182
      |127
      |219
      |299
      |277
      |309
      |576""".stripMargin

  def getPuzzleInput: String = Source.fromResource("day9.txt").getLines().mkString("\n")
}
