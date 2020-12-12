package de.emiliomg.adventofcode.y2020

import scala.annotation.tailrec

object Day3 {
  def star1(data: Array[String]): Int = {
    traverseMap(data, 3, 1)
  }

  def star2(data: Array[String]): Long = {
    val x = List(
      (1, 1),
      (3, 1),
      (5, 1),
      (7, 1),
      (1, 2)
    ).map(slope => traverseMap(data, slope._1, slope._2)).map(_.toLong)
    x.product
  }

  private def traverseMap(data: Array[String], rightOffset: Int, downOffset: Int): Int = {
    val cols = data(0).length
    val rows = data.length
    assert(data.map(_.length).forall(_ == cols), "each row must have same length")

    @tailrec
    def step(colPos: Int, rowPos: Int, trees: Int): Int = {
      val newColPos = (colPos + rightOffset) % cols
      val newRowPos = rowPos + downOffset

      if (newRowPos > rows) trees
      else {
        data(rowPos).charAt(colPos) match {
          case '#' => step(newColPos, newRowPos, trees + 1)
          case _   => step(newColPos, newRowPos, trees)
        }
      }
    }

    step(0, 0, 0)
  }
}
