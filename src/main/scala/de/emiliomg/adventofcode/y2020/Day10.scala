package de.emiliomg.adventofcode.y2020

object Day10 {
  def star1(raw: String): Long = {
    val adapters: List[Long] = getAdapterList(raw)

    val joltDifferences = adapters.sliding(2).toList.map(x => x.last - x.head).groupBy(identity)
    joltDifferences(1).size * joltDifferences(3).size
  }

  def star2(raw: String): Long = {

    val adapters: List[Long] = getAdapterList(raw)
    val dag = adapters.foldRight(Map[Long, Long]()) { (adapter, storedPossibilities) =>
      val possibilities = adapters.filter(x => List(1, 2, 3).contains(x - adapter))
      if (possibilities.isEmpty) {
        storedPossibilities.updated(adapter, 1)
      } else {
        val weight =
          possibilities.map { a =>
            storedPossibilities.getOrElse(a, throw new Exception("This should not happen"))
          }.sum
        storedPossibilities.updated(adapter, weight)
      }
    }

    dag(0)
  }

  private def getAdapterList(raw: String): List[Long] = {
    val bagAdapters = raw.split("\n").toList.map(_.toLong).sorted
    0L +: bagAdapters :+ bagAdapters.max + 3
  }
}
