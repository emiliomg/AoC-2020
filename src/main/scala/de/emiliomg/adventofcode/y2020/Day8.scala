package de.emiliomg.adventofcode.y2020

import scala.annotation.tailrec

object Day8 {
  def star1(raw: String): Int = {
    @tailrec
    def step(m: Machine, alreadyRun: List[Int]): Int = {
      if (alreadyRun.contains(m.currentPos)) m.acc
      else step(m.step(), alreadyRun.appended(m.currentPos))
    }

    val m = Machine(raw)
    step(m, List())
  }
}

case class Machine(data: Array[(String, Int)], currentPos: Int, acc: Int) {
  def step(): Machine =
    data(currentPos) match {
      case ("nop", _) => Machine(data, currentPos + 1, acc)
      case ("acc", p) => Machine(data, currentPos + 1, acc + p)
      case ("jmp", p) => Machine(data, currentPos + p, acc)
    }
}

object Machine {
  def apply(raw: String): Machine = {
    val data: Array[(String, Int)] = raw
      .split("\n")
      .map { line =>
        val x = line.split(" ")
        x(0) -> x(1).toInt
      }

    Machine(data, currentPos = 0, acc = 0)
  }
}
