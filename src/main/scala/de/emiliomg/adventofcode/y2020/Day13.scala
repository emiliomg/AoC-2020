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

    // a (_ + 1) works as well, but since the first ts HAS to be a multiple of the first bus id, we can save time by increasing in multiples of this id.
    // We add the positional offset as well in case the usable bus id is not the first entry in the list (i.e. the list starts with at lease one "x" that gets discarded)
    // .iterate(0L)(_ + (requiredArrivalOrder.head._1 + requiredArrivalOrder.head._2))
    val result: Long = Iterator
      .iterate(0L)(_ + (requiredArrivalOrder.head._1 + requiredArrivalOrder.head._2))
      .dropWhile { ts =>
        !requiredArrivalOrder.forall {
          case (busId, expectedTimeOffset) =>
            (ts + expectedTimeOffset) % busId == 0
        }
      }
      .take(1)
      .to(Iterable)
      .head

    println("busIds")
    pprint.pprintln(busIds)
    println("result")
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
