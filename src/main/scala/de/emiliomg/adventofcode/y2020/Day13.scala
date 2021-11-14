package de.emiliomg.adventofcode.y2020

object Day13 {
  def star1(raw: String): Int = {
    val (earliestTimestamp: Int, allIds: Seq[String]) = parseData(raw)

    val ids = allIds.collect(_.toIntOption).flatten
    val possibleTs = ids.zip(ids).map {
      case (id, cnt) =>
        val result = Iterator
          .iterate(cnt)(_ + cnt)
          .dropWhile(_ <= earliestTimestamp)
          .take(1)
          .to(Iterable)
          .head
        id -> result
    }

    // val fastestBusWithEarliestTimestamp = possibleTs.minBy { case (x, y) => y }
    val (earliestBusId, earliestBusTimestamp) = possibleTs.minBy { case (x, y) => y }

    (earliestBusTimestamp - earliestTimestamp) * earliestBusId
  }

  def star2(raw: String): Long = {
    val (_, busIds) = parseData(raw)
    val requiredArrivalOrder: Seq[(Int, Int)] =
      busIds.zipWithIndex.filterNot(_._1 == "x").map { case (s, ix) => s.toInt -> ix }

    val result: Long = Iterator
      .iterate(0L)(_ + 1)
      .dropWhile { ts =>
        !requiredArrivalOrder.forall {
          case (busId, expectedTimeOffset) =>
            (ts + expectedTimeOffset) % busId == 0
        }
      }
      .take(1)
      .to(Iterable)
      .head

    pprint.pprintln(busIds)
    pprint.pprintln(result)

    result
  }

  def parseData(raw: String): (Int, Seq[String]) = {
    val tsAndIdString     = raw.split("\n")
    val earliestTimestamp = tsAndIdString(0).toInt
    val busIds            = tsAndIdString(1).split(",").map(_.trim)

    earliestTimestamp -> busIds
  }
}
