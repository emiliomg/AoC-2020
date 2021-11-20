package de.emiliomg.adventofcode.y2020

import scala.util.Try

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

  case class BusDeparture(id: Int, tsOffset: Int)

  def star2(raw: String): Long = {

    // 3417L
    // Array(BusDeparture(17, 0), BusDeparture(13, 2), BusDeparture(19, 3))
    def step(busArrivals: Array[BusDeparture], busPtr: Int, curTs: Long, stepSize: Long): Long = {

      // println(s"busPrt: $busPtr, curTs: $curTs, stepSize: $stepSize")

      // if (curTs > 3417) {
      //   println("oh noes")
      //   System.exit(0)
      // }

      val isMatch = busArrivals.forall {
        case BusDeparture(id, tsOffset) =>
          (curTs + tsOffset) % id == 0
      }

      if (isMatch) {
        println(s"SUCCESS: $curTs")
        curTs
      } else {
        val curBus: BusDeparture             = busArrivals(busPtr)
        val nextBusOpt: Option[BusDeparture] = Try(busArrivals(busPtr + 1)).toOption

        nextBusOpt match {
          case None =>
            step(busArrivals, busPtr, curTs + stepSize, stepSize)
          case Some(nextBus) =>
            if ((curTs + nextBus.tsOffset) % nextBus.id == 0) {
              step(busArrivals, busPtr + 1, curTs + nextBus.id, nextBus.id)
            } else {
              step(busArrivals, busPtr, curTs + stepSize, stepSize)
            }
        }
      }
    }

    val (_, busIds) = parseData(raw)
    val requiredArrivalOrder: Array[BusDeparture] =
      busIds.zipWithIndex
        .filterNot(_._1 == "x")
        .map { case (id, tsOffset) => BusDeparture(id.toInt, tsOffset) }
        .toArray

    // pprint.pprintln(requiredArrivalOrder)

    val result: Long = step(requiredArrivalOrder, 0, 0, requiredArrivalOrder(0).id)

    result
  }

  def parseData(raw: String): (Int, Seq[String]) = {
    val tsAndIdString     = raw.split("\n")
    val earliestTimestamp = tsAndIdString(0).toInt
    val busIds            = tsAndIdString(1).split(",").map(_.trim)

    earliestTimestamp -> busIds
  }
}
