package de.emiliomg.adventofcode.y2020

object Day6 {
  def star1(data: String): Int = {
    val groups = data.split("\n\n")
    groups.map(_.split("\n").mkString.toSet.size).sum
  }
}
