package de.emiliomg.adventofcode.y2020

import de.emiliomg.adventofcode.y2020.Day12.Direction
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.scalatest.prop.{TableDrivenPropertyChecks, TableFor4}

import scala.io.Source

class Day12Test extends AnyFlatSpec with Matchers with TableDrivenPropertyChecks {

  "Directions" should "work properly" in {
    forAll(testDirections) { (start, turn, deg, exp) =>
      val result = if (turn == "left") {
        start.turnLeft(deg)
      } else {
        start.turnRight(deg)
      }

      result shouldEqual exp
    }
  }

  "Star1" should "work with the test input" in {
    Day12.star1(getTestInput) shouldEqual 25
  }

  it should "work with the puzzle input" in {
    val result = Day12.star1(getPuzzleInput)
    println(s"Day12, Star1: $result")
  }

  // Todo: Use Property driven testing (Scalacheck?)
  def testDirections: TableFor4[Direction, String, Int, Direction] =
    Table(
      ("start", "turn", "degree", "expectation"),
      (Direction.North, "left", 90, Direction.West),
      (Direction.North, "left", 180, Direction.South),
      (Direction.North, "left", 270, Direction.East),
      (Direction.North, "left", 360, Direction.North),
      (Direction.East, "left", 90, Direction.North),
      (Direction.East, "left", 180, Direction.West),
      (Direction.East, "left", 270, Direction.South),
      (Direction.East, "left", 360, Direction.East),
      (Direction.South, "left", 90, Direction.East),
      (Direction.South, "left", 180, Direction.North),
      (Direction.South, "left", 270, Direction.West),
      (Direction.South, "left", 360, Direction.South),
      (Direction.West, "left", 90, Direction.South),
      (Direction.West, "left", 180, Direction.East),
      (Direction.West, "left", 270, Direction.North),
      (Direction.West, "left", 360, Direction.West),
      (Direction.North, "right", 90, Direction.East),
      (Direction.North, "right", 180, Direction.South),
      (Direction.North, "right", 270, Direction.West),
      (Direction.North, "right", 360, Direction.North),
      (Direction.East, "right", 90, Direction.South),
      (Direction.East, "right", 180, Direction.West),
      (Direction.East, "right", 270, Direction.North),
      (Direction.East, "right", 360, Direction.East),
      (Direction.South, "right", 90, Direction.West),
      (Direction.South, "right", 180, Direction.North),
      (Direction.South, "right", 270, Direction.East),
      (Direction.South, "right", 360, Direction.South),
      (Direction.West, "right", 90, Direction.North),
      (Direction.West, "right", 180, Direction.East),
      (Direction.West, "right", 270, Direction.South),
      (Direction.West, "right", 360, Direction.West)
    )

  def getTestInput: String =
    """F10
      |N3
      |F7
      |R90
      |F11""".stripMargin

  def getPuzzleInput: String = Source.fromResource("day12.txt").getLines().mkString("\n")
}
