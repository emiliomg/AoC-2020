package de.emiliomg.adventofcode.y2020

import scala.annotation.tailrec

object Day9 {
  def star1(raw: String, preambleLength: Int): Long = {
    @tailrec
    def getResult(data: Array[Long], currentPos: Int, preambleLength: Int): Long = {
      if (currentPos > data.length) throw new Exception("Exceeding data, this should not happen")

      val preamble = data.slice(currentPos - preambleLength, currentPos)
      val checkMe  = data(currentPos)

      // split into two steps for better debugging
      val checks: Array[(Boolean, (Long, Long))] = for {
        a <- preamble
        b <- preamble
        if a != b
      } yield (a + b == checkMe) -> (a, b)

      val isValid = checks.exists(_._1)

      if (isValid) getResult(data, currentPos + 1, preambleLength)
      else checkMe

    }

    val data = raw.split("\n").map(_.toLong)
    getResult(data, preambleLength, preambleLength)
  }
}
