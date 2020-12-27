package de.emiliomg.adventofcode.y2020

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import scala.io.Source

class Day7Test extends AnyFlatSpec with Matchers {
  "Star 1" should "work with test data" in {
    Day7.star1(getTestInput) shouldEqual 4
  }

  it should "work with the puzzle input" in {
    val result = Day7.star1(getPuzzleInput)
    println(s"Day7, Star1: $result")
    result shouldEqual 192
  }

  def getTestInput: String =
    """light red bags contain 1 bright white bag, 2 muted yellow bags.
      |dark orange bags contain 3 bright white bags, 4 muted yellow bags.
      |bright white bags contain 1 shiny gold bag.
      |muted yellow bags contain 2 shiny gold bags, 9 faded blue bags.
      |shiny gold bags contain 1 dark olive bag, 2 vibrant plum bags.
      |dark olive bags contain 3 faded blue bags, 4 dotted black bags.
      |vibrant plum bags contain 5 faded blue bags, 6 dotted black bags.
      |faded blue bags contain no other bags.
      |dotted black bags contain no other bags.""".stripMargin

  def getPuzzleInput: String = Source.fromResource("day7.txt").getLines().mkString("\n")
}
