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

  def star2(raw: String): Int = {
    case object MachineLoopException extends Exception

    def runMachine(initialMachine: Machine): Int = {
      @tailrec
      def step(m: Machine, alreadyRun: List[Int]): Int = {
        if (alreadyRun.contains(m.currentPos)) throw MachineLoopException
        else if (m.currentPos == m.data.length) m.acc
        else step(m.step(), alreadyRun.appended(m.currentPos))
      }
      step(initialMachine, List())
    }

    @tailrec
    def modifyMachine(initialMachine: Machine, modifyStepFromHere: Int): (Machine, Int) = {
      initialMachine.data(modifyStepFromHere) match {
        case ("nop", p) =>
          val newMachine = initialMachine.copy(data = initialMachine.data.updated(modifyStepFromHere, "jmp" -> p))
          newMachine -> modifyStepFromHere
        case ("jmp", p) =>
          val newMachine = initialMachine.copy(data = initialMachine.data.updated(modifyStepFromHere, "nop" -> p))
          newMachine -> modifyStepFromHere
        case _ => modifyMachine(initialMachine, modifyStepFromHere + 1)
      }
    }

    def run(initialMachine: Machine, modifyStepFromHere: Int): (Int, Int) = {
      if (modifyStepFromHere > initialMachine.data.length)
        throw new Exception("all modification exhausted, this should not happen")
      val (newMachine, modifiedStep) = modifyMachine(initialMachine, modifyStepFromHere)

      try {
        val result = runMachine(newMachine)
        result -> modifiedStep
      } catch {
        case MachineLoopException => run(initialMachine, modifiedStep + 1)
      }
    }

    val m                      = Machine(raw)
    val (result, modifiedStep) = run(m, 0)
    println(s"Result $result after modifying step $modifiedStep")

    result
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
