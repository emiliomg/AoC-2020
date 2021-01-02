package de.emiliomg.adventofcode.y2020

import scala.annotation.tailrec

object Day11 {
  type SeatLayout = Array[Array[Char]]

  def star1(raw: String): Int = {
    def numOccupiedSurroundings(row: Int, col: Int, seats: SeatLayout): Int = {
      (for {
        r <- (row - 1).to(row + 1)
        c <- (col - 1).to(col + 1)
      } yield {
        (r, c) match {
          case (r, c)
              if !(r == row && c == col) &&
                seats.indices.contains(r) &&
                seats(0).indices.contains(c) &&
                seats(r)(c) == '#' =>
            Some(1)
          case _ => None
        }
      }).flatten.sum
    }

    val seats: SeatLayout = raw.split("\n").map(_.toCharArray)
    runSeats(seats, 4, numOccupiedSurroundings).map(_.count(_ == '#')).sum
  }

  def star2(raw: String): Int = {
    def numOccupiedSurroundings(row: Int, col: Int, seats: SeatLayout): Int = {
      @tailrec
      def isOccupiedSeatInDirection(rowPos: Int, colPos: Int, rowOffset: Int, colOffset: Int): Boolean = {
        val newRowPos = rowPos + rowOffset
        val newColPos = colPos + colOffset

        if (!seats.indices.contains(newRowPos) || !seats(0).indices.contains(newColPos)) false
        else if (seats(newRowPos)(newColPos) == 'L') false
        else if (seats(newRowPos)(newColPos) == '#') true
        else isOccupiedSeatInDirection(newRowPos, newColPos, rowOffset, colOffset)
      }

      (for {
        rowOffset <- -1 to 1
        colOffset <- -1 to 1
      } yield {
        (rowOffset, colOffset) match {
          case (rowOffset, colOffset)
              if !(rowOffset == 0 && colOffset == 0) && isOccupiedSeatInDirection(row, col, rowOffset, colOffset) =>
            Some(1)
          case _ => None
        }
      }).flatten.sum
    }

    val seats: SeatLayout = raw.split("\n").map(_.toCharArray)
    runSeats(seats, 5, numOccupiedSurroundings).map(_.count(_ == '#')).sum
  }

  @tailrec
  def runSeats(
    seats: SeatLayout,
    thresholdToEmpty: Int,
    numOccupiedSurroundings: (Int, Int, SeatLayout) => Int
  ): SeatLayout = {
    val newSeats: SeatLayout = {
      seats.zipWithIndex.map {
        case (row, iRow) =>
          row.zipWithIndex.map {
            case (seat, iCol) if seat == 'L' && numOccupiedSurroundings(iRow, iCol, seats) == 0                => '#'
            case (seat, iCol) if seat == '#' && numOccupiedSurroundings(iRow, iCol, seats) >= thresholdToEmpty => 'L'
            case (seat, _)                                                                                     => seat
          }
      }
    }

    if (newSeats.deepCompare(seats)) seats
    else runSeats(newSeats, thresholdToEmpty, numOccupiedSurroundings)
  }

  implicit class RichSeatLayout(self: SeatLayout) {
    // Only to be used for a known form of string arrays, otherwise collisions and ambiguity can happen!
    def deepCompare(other: SeatLayout): Boolean = {
      val selfString  = self.map(_.mkString("--")).mkString("nl")
      val otherString = other.map(_.mkString("--")).mkString("nl")

      selfString == otherString
    }

    def pretty: String = {
      self.map(_.mkString("")).mkString("\n")
    }
  }
}
