package de.emiliomg.adventofcode.y2020

import scala.annotation.tailrec

object Day12 {
  def star1(raw: String): Int = {
    val directions = parseData(raw)
    processDirections(directions)
  }

  def parseData(raw: String): List[Instruction] = {
    raw.split("\n").toList.map(Instruction.apply)
  }

  def processDirections(initialInstructions: List[Instruction]): Int = {
    @tailrec
    def step(data: List[Instruction], ship: Ship): Int =
      data match {
        case Nil =>
          math.abs(ship.position.xPos) + math.abs(ship.position.yPos)
        case nextInstruction :: tail => step(tail, ship.runInstruction(nextInstruction))
      }

    step(initialInstructions, Ship(90, Position(0, 0)))
  }
}

case class Ship(degree: Int, position: Position) {
  val allowedDegrees: List[Int] = List(0, 90, 180, 270, 360)

  def runInstruction(instruction: Instruction): Ship =
    instruction match {
      case Instruction(Action.MoveNorth, p)   => moveNorth(p)
      case Instruction(Action.MoveSouth, p)   => moveSouth(p)
      case Instruction(Action.MoveEast, p)    => moveEast(p)
      case Instruction(Action.MoveWest, p)    => moveWest(p)
      case Instruction(Action.RotateLeft, p)  => turnLeft(p)
      case Instruction(Action.RotateRight, p) => turnRight(p)
      case Instruction(Action.MoveForward, p) => {
        degree match {
          case 0   => moveNorth(p)
          case 180 => moveSouth(p)
          case 90  => moveEast(p)
          case 270 => moveWest(p)
        }
      }
    }

  private def moveNorth(p: Int): Ship = this.copy(position = position.copy(xPos = position.xPos + p))
  private def moveSouth(p: Int): Ship = this.copy(position = position.copy(xPos = position.xPos - p))
  private def moveEast(p: Int): Ship  = this.copy(position = position.copy(yPos = position.yPos + p))
  private def moveWest(p: Int): Ship  = this.copy(position = position.copy(yPos = position.yPos - p))

  private def turnLeft(deg: Int): Ship = {
    if (!allowedDegrees.contains(deg)) throw new Exception(s"Turn by $deg not allowed!")
    val newDeg = {
      val tmp = degree - deg
      if (tmp < 0) 360 + tmp
      else tmp
    }

    Ship(newDeg, position)
  }

  private def turnRight(deg: Int): Ship = {
    if (!allowedDegrees.contains(deg)) throw new Exception(s"Turn by $deg not allowed!")
    val newDeg = (degree + deg) % 360

    Ship(newDeg, position)
  }
}

case class Instruction(command: Action, parameter: Int)

object Instruction {
  def apply(raw: String): Instruction = {
    val (rc, rp) = raw.splitAt(1)
    val p        = rp.toInt
    rc match {
      case "N" => new Instruction(Action.MoveNorth, p)
      case "S" => new Instruction(Action.MoveSouth, p)
      case "E" => new Instruction(Action.MoveEast, p)
      case "W" => new Instruction(Action.MoveWest, p)
      case "L" => new Instruction(Action.RotateLeft, p)
      case "R" => new Instruction(Action.RotateRight, p)
      case "F" => new Instruction(Action.MoveForward, p)
    }
  }
}

sealed trait Action
object Action {
  case object MoveNorth   extends Action
  case object MoveSouth   extends Action
  case object MoveEast    extends Action
  case object MoveWest    extends Action
  case object RotateLeft  extends Action
  case object RotateRight extends Action
  case object MoveForward extends Action
}

case class Position(xPos: Int, yPos: Int)
