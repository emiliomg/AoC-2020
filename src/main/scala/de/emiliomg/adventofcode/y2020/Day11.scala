package de.emiliomg.adventofcode.y2020

import scala.annotation.tailrec

object Day11 {
  type SeatLayout = Array[Array[Char]]

  def star1(raw: String): Int = {
    val seats: SeatLayout = raw.split("\n").map(_.toCharArray)
    runSeats(seats).map(_.count(_ == '#')).sum
  }

  @tailrec
  def runSeats(seats: SeatLayout): SeatLayout = {
    val newSeats: SeatLayout = {
      seats.zipWithIndex.map {
        case (row, iRow) =>
          row.zipWithIndex.map {
            case (seat, iCol) if seat == 'L' && seat.numOccupiedSurroundings(iRow, iCol, seats) == 0 => '#'
            case (seat, iCol) if seat == '#' && seat.numOccupiedSurroundings(iRow, iCol, seats) >= 4 => 'L'
            case (seat, _)                                                                           => seat
          }
      }
    }

    if (newSeats.deepCompare(seats)) seats
    else runSeats(newSeats)
  }

  implicit class RichSeat(self: Char) {
    def numOccupiedSurroundings(row: Int, col: Int, seats: SeatLayout): Int = {
      (for {
        r <- (row - 1).to(row + 1)
        c <- (col - 1).to(col + 1)
      } yield {
        (r, c) match {
          case (r, c)
              if !(r == row && c == col) &&
                r >= 0 &&
                r < seats.length &&
                c >= 0 &&
                c < seats(0).length &&
                seats(r)(c) == '#' =>
            Some(1)
          case _ => None
        }
      }).flatten.sum
    }
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
