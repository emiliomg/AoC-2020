package de.emiliomg.adventofcode.y2020

import fastparse._
import NoWhitespace._

object Day4 {
  def star1(data: String): Int = {
    parse(data, parseBatch(_)) match {
      case Parsed.Success(result, _) =>
//        println(result)
        result.map(_.isValid).count(identity)
      case Parsed.Failure(_, _, extra) =>
        throw new Exception(extra.trace().longAggregateMsg)
    }
  }

  def parseBatch[_: P]: P[Seq[Passport]] = {
    Start ~ passports ~ End
  }
  def passports[_: P]: P[Seq[Passport]] =
    (passport ~ ("\n\n" ~ passport).rep.?).map {
      case (head, Some(tail)) => head +: tail
      case (head, None)       => Seq(head)
    }

  def passport[_: P]: P[Passport] =
    (passportContent ~ ((" " | "\n") ~ passportContent).rep(1).?)
      .map {
        case (head, Some(tail)) => head +: tail
        case (head, None)       => Seq(head)
      }
      .map(Passport)
  def passportContent[_: P]: P[PassportContent] =
    expirationYear | birthYear | issueYear | height | hairColor | eyeColor | passportId | countryId

  def intNum[_: P]: P[Int]   = CharIn("0-9").rep(1).!.map(Integer.parseInt)
  def color[_: P]: P[String] = ("#".? ~ CharIn("0-9a-fA-F").rep(min = 6, max = 6) | CharIn("a-zA-Z").rep(1)).!

  def birthYear[_: P]: P[BirthYear]           = P("byr:" ~ intNum.map(BirthYear))
  def issueYear[_: P]: P[IssueYear]           = P("iyr:" ~ intNum.map(IssueYear))
  def expirationYear[_: P]: P[ExpirationYear] = P("eyr:" ~ intNum.map(ExpirationYear))
  def height[_: P]: P[Height]                 = P("hgt:" ~ intNum.map(Height) ~ CharIn("a-zA-Z").rep.?)
  def hairColor[_: P]: P[HairColor]           = P("hcl:" ~ color.!.map(HairColor))
  def eyeColor[_: P]: P[EyeColor]             = P("ecl:" ~ color.!.map(EyeColor))
  def passportId[_: P]: P[PassportId]         = P("pid:" ~ CharIn("0-9a-zA-Z#").rep(1).!.map(PassportId))
  def countryId[_: P]: P[CountryId]           = P("cid:" ~ CharIn("0-9").rep(1).!.map(CountryId))

  // todo How to do this with types?
  case class Passport(content: Seq[PassportContent]) {
    def isValid: Boolean = {
      val mandatoryFields = Map(
        BirthYear      -> false,
        IssueYear      -> false,
        ExpirationYear -> false,
        Height         -> false,
        HairColor      -> false,
        EyeColor       -> false,
        PassportId     -> false
//        CountryId      -> false
      )

      val checked = content.foldLeft(mandatoryFields) { (check, c) =>
        c match {
          case BirthYear(_)      => check.updated(BirthYear, true)
          case IssueYear(_)      => check.updated(IssueYear, true)
          case ExpirationYear(_) => check.updated(ExpirationYear, true)
          case Height(_)         => check.updated(Height, true)
          case HairColor(_)      => check.updated(HairColor, true)
          case EyeColor(_)       => check.updated(EyeColor, true)
          case PassportId(_)     => check.updated(PassportId, true)
//          case CountryId(_)      => check.updated(CountryId, true) // todo see exception
          case _ => check
        }
      }

      checked.values.forall(_ == true)
    }
  }

  sealed trait PassportContent
  case class BirthYear(year: Int)      extends PassportContent
  case class IssueYear(year: Int)      extends PassportContent
  case class ExpirationYear(year: Int) extends PassportContent
  case class Height(height: Int)       extends PassportContent // ignoring the unit
  case class HairColor(rgb: String)    extends PassportContent
  case class EyeColor(color: String)   extends PassportContent
  case class PassportId(id: String)    extends PassportContent
  case class CountryId(id: String)     extends PassportContent
}
