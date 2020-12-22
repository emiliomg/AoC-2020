package de.emiliomg.adventofcode.y2020

import scala.annotation.tailrec

object Day5 {
  def star1(data: List[String]): Int = {
    getSeatIds(data).max
  }

  def star2(data: List[String]): Int = {
    @tailrec
    def step(ids: List[Int]): Int =
      ids match {
        case _ :: Nil                  => throw new Exception("End of list reached, this should not happen")
        case x :: y :: _ if x + 1 != y => x + 1
        case _ :: tail                 => step(tail)
      }
    val seatIds: List[Int] = getSeatIds(data).sorted
    step(seatIds)
  }

  def getSeatIds(data: List[String]): List[Int] = {
    data.map { pass =>
      val row = getRow(pass.substring(0, 7))
      val col = getColumn(pass.substring(7))
      (row * 8) + col
    }
  }

  def getRow(input: String): Int = {
    @tailrec
    def step(data: List[String], range: Range): Int =
      data match {
        case Nil if range.head == range.end => range.head
        case Nil                            => throw new Exception(s"Empty steps, but range ambiguous: $range")
        case next :: tail if next == "B"    => step(tail, stepRange(range, getUpperHalf = true))
        case next :: tail if next == "F"    => step(tail, stepRange(range, getUpperHalf = false))
        case zomg :: _                      => throw new Exception(s"Next step $zomg not supported")
      }
    step(input.split("").toList, 0.to(127))
  }

  def getColumn(input: String): Int = {
    @tailrec
    def step(data: List[String], range: Range): Int =
      data match {
        case Nil if range.head == range.end => range.head
        case Nil                            => throw new Exception(s"Empty steps, but range ambiguous: $range")
        case next :: tail if next == "R"    => step(tail, stepRange(range, getUpperHalf = true))
        case next :: tail if next == "L"    => step(tail, stepRange(range, getUpperHalf = false))
        case zomg :: _                      => throw new Exception(s"Next step $zomg not supported")
      }
    step(input.split("").toList, 0.to(7))
  }

  def stepRange(input: Range, getUpperHalf: Boolean): Range = {
    if (getUpperHalf) input.slice(input.size / 2, input.size)
    else input.slice(0, input.size / 2)
  }
}
