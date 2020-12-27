package de.emiliomg.adventofcode.y2020

import fastparse._
import NoWhitespace._

object Day4 {
  def star1(data: String): Int = {
    def validation(content: Seq[PassportContent]): Seq[Boolean] = {
      val mandatoryFields = Map(
        BirthYear      -> false,
        IssueYear      -> false,
        ExpirationYear -> false,
        Height         -> false,
        HairColor      -> false,
        EyeColor       -> false,
        PassportId     -> false
      )

      val checked = content.foldLeft(mandatoryFields) { (check, c) =>
        c match {
          case BirthYear(_)      => check.updated(BirthYear, true)
          case IssueYear(_)      => check.updated(IssueYear, true)
          case ExpirationYear(_) => check.updated(ExpirationYear, true)
          case Height(_, _)      => check.updated(Height, true)
          case HairColor(_)      => check.updated(HairColor, true)
          case EyeColor(_)       => check.updated(EyeColor, true)
          case PassportId(_)     => check.updated(PassportId, true)
          case _                 => check
        }
      }

      checked.values.toSeq
    }

    runData(data, validation)
  }

  def star2(data: String): Int = {
    def validation(content: Seq[PassportContent]): Seq[Boolean] = {
      val mandatoryFields = Map(
        BirthYear      -> false,
        IssueYear      -> false,
        ExpirationYear -> false,
        Height         -> false,
        HairColor      -> false,
        EyeColor       -> false,
        PassportId     -> false
      )

      val checked = content.foldLeft(mandatoryFields) { (check, c) =>
        c match {
          case BirthYear(y) if y.length == 4 && 1920.to(2002).contains(y.toInt) =>
            check.updated(BirthYear, true)
          case IssueYear(y) if y.length == 4 && 2010.to(2020).contains(y.toInt) =>
            check.updated(IssueYear, true)
          case ExpirationYear(y) if y.length == 4 && 2020.to(2030).contains(y.toInt) =>
            check.updated(ExpirationYear, true)
          case Height(len, unit) if unit == "cm" && 150.to(193).contains(len.toInt) =>
            check.updated(Height, true)
          case Height(len, unit) if unit == "in" && 59.to(76).contains(len.toInt) =>
            check.updated(Height, true)
          case HairColor(color) if color.charAt(0) == '#' && color.length == 7 =>
            check.updated(HairColor, true)
          case EyeColor(color) if List("amb", "blu", "brn", "gry", "grn", "hzl", "oth").contains(color) =>
            check.updated(EyeColor, true)
          case PassportId(pid) if pid.length == 9 =>
            check.updated(PassportId, true)
          case _ => check
        }
      }

      checked.values.toSeq
    }

    runData(data, validation)
  }

  private def runData(data: String, validation: Seq[PassportContent] => Seq[Boolean]) = {
    parse(data, parseBatch(_)) match {
      case Parsed.Success(result, _) =>
        result.map(_.isValid(validation)).count(identity)
      case Parsed.Failure(_, _, extra) =>
        throw new Exception(extra.trace().longAggregateMsg)
    }
  }

  def parseBatch[_: P]: P[Seq[Passport]] = {
    Start ~ passports ~ End
  }
  def passports[_: P]: P[Seq[Passport]] =
    passport.rep(min = 1, sep = "\n\n")

  def passport[_: P]: P[Passport] =
    passportContent.rep(sep = "\n").rep(sep = " ").map(p => Passport(p.flatten))

  def passportContent[_: P]: P[PassportContent] =
    expirationYear | birthYear | issueYear | height | hairColor | eyeColor | passportId | countryId

  def intNum[_: P]: P[String] = CharIn("0-9").rep(1).!
  def color[_: P]: P[String] =
    ("#".?.! ~ CharIn("0-9a-fA-F").rep(min = 6, max = 6) | CharIn("a-zA-Z").rep(1)).!

  def birthYear[_: P]: P[BirthYear]           = P("byr:" ~ intNum.map(BirthYear))
  def issueYear[_: P]: P[IssueYear]           = P("iyr:" ~ intNum.map(IssueYear))
  def expirationYear[_: P]: P[ExpirationYear] = P("eyr:" ~ intNum.map(ExpirationYear))
  def height[_: P]: P[Height]                 = P("hgt:" ~ (intNum ~ CharIn("a-zA-Z").rep.?.!).map(x => Height(x._1, x._2)))
  def hairColor[_: P]: P[HairColor]           = P("hcl:" ~ color.map(HairColor))
  def eyeColor[_: P]: P[EyeColor]             = P("ecl:" ~ color.!.map(EyeColor))
  def passportId[_: P]: P[PassportId]         = P("pid:" ~ CharIn("0-9a-zA-Z#").rep(1).!.map(PassportId))
  def countryId[_: P]: P[CountryId]           = P("cid:" ~ CharIn("0-9").rep(1).!.map(CountryId))

  case class Passport(content: Seq[PassportContent]) {
    def isValid(f: Seq[PassportContent] => Seq[Boolean]): Boolean = {
      f(content).forall(_ == true)
    }
  }

  sealed trait PassportContent
  case class BirthYear(year: String)              extends PassportContent
  case class IssueYear(year: String)              extends PassportContent
  case class ExpirationYear(year: String)         extends PassportContent
  case class Height(height: String, unit: String) extends PassportContent
  case class HairColor(color: String)             extends PassportContent
  case class EyeColor(color: String)              extends PassportContent
  case class PassportId(id: String)               extends PassportContent
  case class CountryId(id: String)                extends PassportContent
}
