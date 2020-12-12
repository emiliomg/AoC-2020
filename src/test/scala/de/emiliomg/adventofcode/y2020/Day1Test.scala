package de.emiliomg.adventofcode.y2020

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import scala.io.Source

class Day1Test extends AnyFlatSpec with Matchers {
  "Star 1" should "work with the test input" in {
    Day1.star1(getTestInput) shouldEqual 514579
  }

  it should "work with the puzzle input" in {
    val result = Day1.star1(getPuzzleInput)
    println(result)
    result shouldEqual 539851
  }

  "Star 2" should "work with the test input" in {
    Day1.star2(getTestInput) shouldEqual 241861950
  }

  it should "work with the puzzle input" in {
    val result = Day1.star2(getPuzzleInput)
    println(result)
    result shouldEqual 212481360
  }

  private def getTestInput: List[Int] = {
    List(1721, 979, 366, 299, 675, 1456)
  }

  private def getPuzzleInput: List[Int] = {
    Source.fromResource("day1.txt").getLines().toList.map(_.toInt)
  }
}
