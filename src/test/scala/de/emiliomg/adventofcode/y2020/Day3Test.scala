package de.emiliomg.adventofcode.y2020

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import scala.io.Source

class Day3Test extends AnyFlatSpec with Matchers {

  "Star 1" should "work with test input" in {
    Day3.star1(getTestInput) shouldEqual 7
  }

  it should "work with the puzzle input" in {
    val result = Day3.star1(getPuzzleInput)
    println(s"Star1 result = $result")
    result shouldEqual 272
  }

  "Star 2" should "work with the test input" in {
    Day3.star2(getTestInput) shouldEqual 336
  }

  it should "work with the puzzle input" in {
    val result = Day3.star2(getPuzzleInput)
    println(s"Star2 result = $result")
    result shouldEqual 3898725600L
  }

  def getTestInput: Array[String] = {
    """..##.......
      |#...#...#..
      |.#....#..#.
      |..#.#...#.#
      |.#...##..#.
      |..#.##.....
      |.#.#.#....#
      |.#........#
      |#.##...#...
      |#...##....#
      |.#..#...#.#""".stripMargin.split("\n")
  }

  def getPuzzleInput: Array[String] = {
    Source.fromResource("day3.txt").getLines().toArray
  }
}
