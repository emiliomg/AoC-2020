package de.emiliomg.adventofcode.y2020

object Day13 {
  def star1(raw: String): Int = {
    val (ts: Int, allIds: Seq[String]) = parseData(raw)

    val ids = allIds.collect(_.toIntOption).flatten
    val possibleTs = ids.zip(ids).map {
      case (id, cnt) =>
        val result = Iterator
          .iterate(cnt)(_ + cnt)
          .dropWhile(_ <= ts)
          .take(1)
          .to(Iterable)
          .head
        id -> result
    }

    val fastestBusWithEarliestTimestamp = possibleTs.minBy { case (x, y) => y }

    (fastestBusWithEarliestTimestamp._2 - ts) * fastestBusWithEarliestTimestamp._1
  }

  def parseData(raw: String): (Int, Seq[String]) = {
    val tsAndIdString = raw.split("\n")
    val ts            = tsAndIdString(0).toInt
    val ids           = tsAndIdString(1).split(",").map(_.trim)

    ts -> ids
  }
}
