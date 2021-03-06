package de.emiliomg.adventofcode.y2020

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.scalatest.prop.TableDrivenPropertyChecks

import scala.io.Source

class Day12Test extends AnyFlatSpec with Matchers with TableDrivenPropertyChecks {

  "Star1" should "work with the test input" in {
    Day12.star1(getTestInput) shouldEqual 25
  }

  it should "work with the puzzle input" in {
    val result = Day12.star1(getPuzzleInput)
    println(s"Day12, Star1: $result")
    result shouldEqual 1424
  }

  "Star2" should "work with the test input" in {
    Day12.star2(getTestInput) shouldEqual 286
  }

  it should "work with the puzzle input" in {
    val result = Day12.star2(getPuzzleInput)
    println(s"Day12, Star2: $result")
    result shouldEqual 63447
  }

  def getTestInput: String =
    """F10
      |N3
      |F7
      |R90
      |F11""".stripMargin

  def getPuzzleInput: String = Source.fromResource("day12.txt").getLines().mkString("\n")
}
