package de.emiliomg.adventofcode.y2020

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import scala.io.Source

class Day10Test extends AnyFlatSpec with Matchers {
  "Star1" should "work with the small test data" in {
    Day10.star1(getSmallTestInput) shouldEqual 7 * 5
  }

  it should "work with large the test data" in {
    Day10.star1(getLargeTestInput) shouldEqual 22 * 10
  }

  it should "work with the puzzle data" in {
    val result = Day10.star1(getPuzzleInput)
    println(s"Day10, Star1: $result")
    result shouldEqual 2738
  }

  "Star2" should "work with the small test data" in {
    Day10.star2(getSmallTestInput) shouldEqual 8
  }

  it should "work with the large test data" in {
    Day10.star2(getLargeTestInput) shouldEqual 19208
  }

  it should "work with the puzzle data" in {
    val result = Day10.star2(getPuzzleInput)
    println(s"Day10, Star2: $result")
    result shouldEqual 74049191673856L
  }

  def getSmallTestInput: String =
    """16
      |10
      |15
      |5
      |1
      |11
      |7
      |19
      |6
      |12
      |4""".stripMargin

  def getLargeTestInput: String =
    """28
      |33
      |18
      |42
      |31
      |14
      |46
      |20
      |48
      |47
      |24
      |23
      |49
      |45
      |19
      |38
      |39
      |11
      |1
      |32
      |25
      |35
      |8
      |17
      |7
      |9
      |4
      |2
      |34
      |10
      |3""".stripMargin

  def getPuzzleInput: String = Source.fromResource("day10.txt").getLines().mkString("\n")
}
