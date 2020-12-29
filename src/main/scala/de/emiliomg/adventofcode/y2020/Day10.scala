package de.emiliomg.adventofcode.y2020

object Day10 {
  def star1(raw: String): Long = {
    val bagAdapters = raw.split("\n").toList.map(_.toLong).sorted
    val adapters    = 0L +: bagAdapters :+ bagAdapters.max + 3

    val joltDifferences = adapters.sliding(2).toList.map(x => x.last - x.head).groupBy(identity)
    joltDifferences(1).size * joltDifferences(3).size
  }
}
