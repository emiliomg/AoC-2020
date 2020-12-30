package de.emiliomg.adventofcode.y2020

import de.emiliomg.adventofcode.y2020.Day11.RichSeatLayout
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.scalatest.prop.{TableDrivenPropertyChecks, TableFor3}

import scala.io.Source

class Day11Test extends AnyFlatSpec with Matchers with TableDrivenPropertyChecks {

  "RichSeatLayout" should "correctly define equality for a seat layout" in {
    forAll(testSeatLayouts) { (a, b, exp) =>
      a.deepCompare(b) shouldBe exp
    }

  }

  "Star1" should "work with the test data" in {
    Day11.star1(getTestInput) shouldEqual 37
  }

  it should "work with the puzzle data" in {
    val result = Day11.star1(getPuzzleInput)
    println(s"Day11, Star1: $result")
    result shouldEqual 2386
  }

  def getTestInput: String =
    """L.LL.LL.LL
      |LLLLLLL.LL
      |L.L.L..L..
      |LLLL.LL.LL
      |L.LL.LL.LL
      |L.LLLLL.LL
      |..L.L.....
      |LLLLLLLLLL
      |L.LLLLLL.L
      |L.LLLLL.LL""".stripMargin

  def getPuzzleInput: String = Source.fromResource("day11.txt").getLines().mkString("\n")

  def testSeatLayouts: TableFor3[Array[Array[Char]], Array[Array[Char]], Boolean] =
    Table(
      ("first", "second", "expectedResult"),
      (Array(Array('#', '#', '#'), Array('#', 'L', '#')), Array(Array('#', '#', '#'), Array('#', 'L', '#')), true),
      (Array(Array('#', '#', '#'), Array('#', 'L', '#')), Array(Array('#', '#', '#'), Array('#', 'L', 'L')), false)
    )
}
