package de.emiliomg.adventofcode.y2020

import fastparse._
import NoWhitespace._

import scala.annotation.tailrec

object Day7 {
  def star1(data: String): Int = {
    @tailrec
    def step(searchFor: Set[String], alreadyFound: Set[Bag], bags: Seq[Bag]): Int = {
      val newSearchFor = bags.filter(_.children.keySet.intersect(searchFor).nonEmpty).toSet
      if (newSearchFor.isEmpty) alreadyFound.size
      else step(newSearchFor.map(_.name), alreadyFound ++ newSearchFor, bags)
    }

    val bags = parseData(data)
    step(Set("shiny gold"), Set(), bags)
  }

  private def parseData(data: String): Seq[Bag] = {
    parse(data, parseBags(_)) match {
      case Parsed.Success(result, _) =>
        result
      case Parsed.Failure(_, _, extra) =>
        throw new Exception(extra.trace().longAggregateMsg)
    }
  }

  def parseBags[_: P]: P[Seq[Bag]] = Start ~ bags ~ End

  def bags[_: P]: P[Seq[Bag]] = (emptyBag | filledBag).rep(sep = "\n")

  def emptyBag[_: P]: P[Bag] = (parentBagDef ~ " contain no other bags.").map { p => Bag(p, Map()) }

  def filledBag[_: P]: P[Bag] =
    (parentBagDef ~ " contain " ~ filledChildrenBagsDef.rep(min = 1, sep = ", ") ~ ".").map { p =>
      Bag(p._1, p._2.toMap)
    }

  def parentBagDef[_: P]: P[String] =
    (color.rep(exactly = 2, sep = " ") ~ " bags").map(_.mkString(" "))

  def filledChildrenBagsDef[_: P]: P[(String, Int)] =
    (CharIn("0-9").rep(1).! ~ " " ~ color.rep(exactly = 2, sep = " ") ~ (" bags" | " bag")).map { p =>
      p._2.mkString(" ") -> p._1.toInt
    }

  def color[_: P]: P[String] = CharIn("a-z").rep.!

  case class Bag(name: String, children: Map[String, Int])
}
