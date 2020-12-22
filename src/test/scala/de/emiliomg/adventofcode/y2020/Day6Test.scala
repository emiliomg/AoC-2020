package de.emiliomg.adventofcode.y2020

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import scala.io.Source

class Day6Test extends AnyFlatSpec with Matchers {
  "Star1" should "work with the test data" in {
    Day6.star1(getTestInput) shouldEqual 11
  }

  it should "work with the puzzle data" in {
    val result = Day6.star1(getPuzzleInput)
    println(s"Day6, Star1: $result")
    result shouldEqual 6799
  }

  "Star2" should "work with the test data" in {
    Day6.star2(getTestInput) shouldEqual 6
  }

  it should "work with the puzzle data" in {
    val result = Day6.star2(getPuzzleInput)
    println(s"Day6, Star2: $result")
    result shouldEqual 3354
  }

  def getPuzzleInput: String = Source.fromResource("day6.txt").getLines().mkString("\n")

  def getTestInput: String =
    """abc
      |
      |a
      |b
      |c
      |
      |ab
      |ac
      |
      |a
      |a
      |a
      |a
      |
      |b""".stripMargin
}
