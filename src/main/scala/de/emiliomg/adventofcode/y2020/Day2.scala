package de.emiliomg.adventofcode.y2020

object Day2 {
  def star1(data: List[String]): Int = {
    data
      .map(PasswordValidation.fromString(_, SledRentalPasswordValidationRule.isValid))
      .count(_.isValid)
  }

  def star2(data: List[String]): Int = {
    data
      .map(PasswordValidation.fromString(_, TobogganPasswordValidationRule.isValid))
      .count(_.isValid)
  }

}

case class PasswordValidation(password: String, isValid: Boolean)

case object PasswordValidation {
  // todo how can I use the type of PasswordValidationRule here?
  def fromString(str: String, rule: (Char, Int, Int) => String => Boolean): PasswordValidation = {
    val Array(rawRule: String, password: String, _*)         = str.split(": ")
    val Array(rawRange: String, rawChar: String, _*)         = rawRule.split(" ")
    val Array(firstNumber: String, secondNumber: String, _*) = rawRange.split("-")

    PasswordValidation(password, rule(rawChar.charAt(0), firstNumber.toInt, secondNumber.toInt)(password))
  }
}

sealed trait PasswordValidationRule {
  def isValid(ruleChar: Char, ruleFirstNum: Int, ruleSecondNum: Int)(password: String): Boolean
}

case object SledRentalPasswordValidationRule extends PasswordValidationRule {
  override def isValid(ruleChar: Char, ruleFirstNum: Int, ruleSecondNum: Int)(password: String): Boolean = {
    val range: Range.Inclusive = Range.inclusive(ruleFirstNum, ruleSecondNum)
    val counter = password.foldLeft(0) { (acc, c) =>
      if (c == ruleChar) acc + 1 else acc
    }
    range.contains(counter)
  }
}

case object TobogganPasswordValidationRule extends PasswordValidationRule {
  override def isValid(ruleChar: Char, ruleFirstNum: Int, ruleSecondNum: Int)(password: String): Boolean = {
    val firstPos   = ruleFirstNum - 1 // rulePos starts at 1
    val secondsPos = ruleSecondNum - 1

    val firstMatch  = password.charAt(firstPos) == ruleChar
    val secondMatch = password.charAt(secondsPos) == ruleChar

    (firstMatch || secondMatch) && !(firstMatch && secondMatch)
  }
}
