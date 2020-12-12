package de.emiliomg.adventofcode.y2020

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import scala.io.Source

class Day4Test extends AnyFlatSpec with Matchers {
  "Star 1" should "work with test input" in {
    Day4.star1(getTestInput) shouldEqual 2
  }

  it should "work with the puzzle input" in {
    val result = Day4.star1(getPuzzleInput)
    println(s"Star1 result = $result")
    result shouldEqual 264
  }
//
//  "Star 2" should "work with the test input" in {
//    Day4.star2(getTestInput) shouldEqual ???
//  }
//
//  it should "work with the puzzle input" in {
//    val result = Day4.star2(getPuzzleInput)
//    println(s"Star2 result = $result")
//    result shouldEqual ???
//  }

  def getTestInput: String =
    """ecl:gry pid:#d50a43 eyr:2020 hcl:#fffffd
      |byr:1937 iyr:2017 cid:147 hgt:183cm
      |
      |iyr:2013 ecl:amb cid:350 eyr:2023 pid:028048884
      |hcl:#cfa07d byr:1929
      |
      |hcl:#ae17e1 iyr:2013
      |eyr:2024
      |ecl:brn pid:760753108 byr:1931
      |hgt:179cm
      |
      |hcl:#cfa07d eyr:2025 pid:166559648
      |iyr:2011 ecl:brn hgt:59in""".stripMargin

  def getPuzzleInput: String = Source.fromResource("day4.txt").getLines().mkString("\n")
}
