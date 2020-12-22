package de.emiliomg.adventofcode.y2020

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.scalatest.prop.{TableDrivenPropertyChecks, TableFor3}

import scala.io.Source

class Day5Test extends AnyFlatSpec with Matchers with TableDrivenPropertyChecks {

  "Star 1" should "work with the test input" in {
    Day5.star1(getTestInput) shouldEqual 820
  }

  it should "work with the puzzle input" in {
    val result = Day5.star1(getPuzzleInput)
    println(s"Day5, Star1: $result")
  }

  "RangeStepper" should "work with the provided samples" in {
    forAll(testSteps) { (startRange, getUpper, resultRange) =>
      Day5.stepRange(startRange, getUpper) shouldEqual resultRange
    }
  }

  "RowCalculator" should "work with test input" in {
    forAll(testPassports) { (boardingPass, expRow, _) =>
      val rowDefinition = boardingPass.substring(0, 7)
      Day5.getRow(rowDefinition) shouldEqual expRow
    }
  }

  "ColumnCalculator" should "work with test input" in {
    forAll(testPassports) { (boardingPass, _, expCol) =>
      val rowDefinition = boardingPass.substring(7)
      Day5.getColumn(rowDefinition) shouldEqual expCol
    }
  }

  val testPassports: TableFor3[String, Int, Int] = Table(
    ("boardingPass", "expectedRow", "expectedColumn"),
    ("FBFBBFFRLR", 44, 5),
    ("BFFFBBFRRR", 70, 7),
    ("FFFBBBFRRR", 14, 7),
    ("BBFFBBFRLL", 102, 4)
  )

  /*
  FBFBBFFRLR
  Start by considering the whole range, rows 0 through 127.
  F means to take the lower half, keeping rows 0 through 63.
  B means to take the upper half, keeping rows 32 through 63.
  F means to take the lower half, keeping rows 32 through 47.
  B means to take the upper half, keeping rows 40 through 47.
  B keeps rows 44 through 47.
  F keeps rows 44 through 45.
  The final F keeps the lower of the two, row 44.
  -----
  Start by considering the whole range, columns 0 through 7.
  R means to take the upper half, keeping columns 4 through 7.
  L means to take the lower half, keeping columns 4 through 5.
  The final R keeps the upper of the two, column 5.
   */
  val testSteps: TableFor3[Range.Inclusive, Boolean, Range.Inclusive] = Table(
    ("startRange", "getUpper", "resultRange"),
    (0.to(127), false, 0.to(63)),
    (0.to(63), true, 32.to(63)),
    (32.to(63), false, 32.to(47)),
    (32.to(47), true, 40.to(47)),
    (40.to(47), true, 44.to(47)),
    (44.to(47), false, 44.to(45)),
    (44.to(45), false, 44.to(44)),
    (0.to(7), true, 4.to(7)),
    (4.to(7), false, 4.to(5)),
    (4.to(5), true, 5.to(5))
  )

  val getTestInput: List[String] = List("FBFBBFFRLR", "BFFFBBFRRR", "FFFBBBFRRR", "BBFFBBFRLL")

  val getPuzzleInput: List[String] = Source.fromResource("day5.txt").getLines().toList
}
