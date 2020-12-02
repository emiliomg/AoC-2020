package de.emiliomg.adventofcode.y2020.day1

object Day1 {
  def star1(data: List[Int]): Int = {
    val result = for {
      a <- data
      b <- data
      if a + b == 2020
    } yield a * b

    result.head
  }
}
