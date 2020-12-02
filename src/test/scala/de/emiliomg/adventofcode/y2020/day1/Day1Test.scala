package de.emiliomg.adventofcode.y2020.day1

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import scala.io.Source

class Day1Test extends AnyFlatSpec with Matchers {
  "Star 1" should "work with the test input" in {
    val sample = List(1721, 979, 366, 299, 675, 1456)
    Day1.star1(sample) shouldEqual 514579
  }

  it should "work with the puzzle input" in {
    val data = getData("day1/input.txt")
    Day1.star1(data) shouldEqual 539851
  }

  private def getData(path: String): List[Int] = {
    Source.fromResource(path).getLines().toList.map(_.toInt)
  }
}
