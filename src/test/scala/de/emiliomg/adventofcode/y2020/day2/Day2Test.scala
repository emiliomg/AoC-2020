package de.emiliomg.adventofcode.y2020.day2

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import scala.io.Source

class Day2Test extends AnyFlatSpec with Matchers {
  "Star 1" should "work with the test input" in {
    Day2.star1(getTestInput) shouldEqual 2
  }

  it should "work with the puzzle input" in {
    val result = Day2.star1(getPuzzleInput)
    println(result)
    result shouldEqual 586
  }

  def getTestInput: List[PasswordValidation] = {
    val raw = List(
      "1-3 a: abcde",
      "1-3 b: cdefg",
      "2-9 c: ccccccccc"
    )
    raw.map(PasswordValidation.fromString)
  }

  def getPuzzleInput: List[PasswordValidation] =
    Source.fromResource("day2/input.txt").getLines().toList.map(PasswordValidation.fromString)
}
