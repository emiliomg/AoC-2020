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

  def processDirections(initialDirections: List[Instruction]): Int = {
    @tailrec
    def step(data: List[Instruction], dir: Direction, pos: Position): Int =
      data match {
        case Nil =>
          math.abs(pos.xPos) + math.abs(pos.yPos)
        case nextStep :: tail =>
          nextStep match {
            case Instruction(Command.North, p) => step(tail, dir, pos.copy(xPos = pos.xPos + p))
            case Instruction(Command.South, p) => step(tail, dir, pos.copy(xPos = pos.xPos - p))
            case Instruction(Command.East, p)  => step(tail, dir, pos.copy(yPos = pos.yPos + p))
            case Instruction(Command.West, p)  => step(tail, dir, pos.copy(yPos = pos.yPos - p))
            case Instruction(Command.Left, p)  => step(tail, dir.turnLeft(p), pos)
            case Instruction(Command.Right, p) => step(tail, dir.turnRight(p), pos)
            case Instruction(Command.Forward, p) => {
              val newInstruction = dir match {
                case Direction.North => Instruction(Command.North, p)
                case Direction.South => Instruction(Command.South, p)
                case Direction.East  => Instruction(Command.East, p)
                case Direction.West  => Instruction(Command.West, p)
              }
              step(newInstruction +: tail, dir, pos)
            }
          }
      }

    step(initialDirections, Direction.East, Position(0, 0))
  }

  case class Instruction(command: Command, parameter: Int)

  object Instruction {
    def apply(raw: String): Instruction = {
      val (rc, rp) = raw.splitAt(1)
      val p        = rp.toInt
      rc match {
        case "N" => new Instruction(Command.North, p)
        case "S" => new Instruction(Command.South, p)
        case "E" => new Instruction(Command.East, p)
        case "W" => new Instruction(Command.West, p)
        case "L" => new Instruction(Command.Left, p)
        case "R" => new Instruction(Command.Right, p)
        case "F" => new Instruction(Command.Forward, p)
      }
    }
  }

  sealed trait Command
  object Command {
    case object North   extends Command
    case object South   extends Command
    case object East    extends Command
    case object West    extends Command
    case object Left    extends Command
    case object Right   extends Command
    case object Forward extends Command
  }

  sealed trait Direction {
    def ownDegree: Int
    def allowedDegrees: List[Int] = List(0, 90, 180, 270, 360)

    def turnLeft(deg: Int): Direction = {
      if (!allowedDegrees.contains(deg)) throw new Exception(s"Turn by $deg not allowed!")
      val newDeg = ownDegree - deg

      if (newDeg < 0) Direction(360 + newDeg)
      else Direction(newDeg)
    }

    def turnRight(deg: Int): Direction = {
      if (!allowedDegrees.contains(deg)) throw new Exception(s"Turn by $deg not allowed!")
      val newDeg = (ownDegree + deg) % 360
      Direction(newDeg)
    }
  }
  object Direction {
    def apply(deg: Int): Direction =
      deg match {
        case 0   => North
        case 90  => East
        case 180 => South
        case 270 => West
        case x   => throw new Exception(s"Direction for degree $x not defined!")
      }

    case object North extends Direction {
      override def ownDegree: Int = 0
    }
    case object East extends Direction {
      override def ownDegree: Int = 90
    }
    case object South extends Direction {
      override def ownDegree: Int = 180
    }
    case object West extends Direction {
      override def ownDegree: Int = 270
    }
  }

  case class Position(xPos: Int, yPos: Int)
}
