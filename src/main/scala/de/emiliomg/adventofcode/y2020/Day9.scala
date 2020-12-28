package de.emiliomg.adventofcode.y2020

import scala.annotation.tailrec

object Day9 {
  def star1(raw: String, preambleLength: Int): Long = {
    val data = raw.split("\n").map(_.toLong)
    star1(data, preambleLength)
  }

  def star1(data: Array[Long], preambleLength: Int): Long = {
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

    getResult(data, preambleLength, preambleLength)
  }

  def star2(raw: String, preambleLength: Int): Long = {
    @tailrec
    def getContiguousList(data: Array[Long], checkFor: Long, start: Int, end: Int): Seq[Long] = {
      val checkRange   = for (n <- start.to(end)) yield data(n)
      val newSum: Long = checkRange.sum

      if (newSum == checkFor) checkRange
      else if (newSum > checkFor) getContiguousList(data, checkFor, start + 1, end)
      else getContiguousList(data, checkFor, start, end + 1)
    }

    val data          = raw.split("\n").map(_.toLong)
    val invalidNumber = star1(data, preambleLength)

    val checkRange: Seq[Long] = getContiguousList(data, invalidNumber, 0, 0)
    checkRange.min + checkRange.max
  }
}
