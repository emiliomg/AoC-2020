package de.emiliomg.adventofcode.y2020

object Day6 {
  def star1(data: String): Int = {
    val groups = data.split("\n\n")
    groups.map(_.split("\n").mkString.toSet.size).sum
  }

  def star2(data: String): Int = {
    val groups = data.split("\n\n")
    val unanimousVotes = groups.map { group =>
      val persons = group.split("\n").toList.map(_.toSet)
      persons.reduceLeft { (a, b) => a.intersect(b) }
    }
    unanimousVotes.map(_.size).sum
  }
}
