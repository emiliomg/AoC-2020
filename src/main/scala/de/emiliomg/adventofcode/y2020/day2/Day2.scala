package de.emiliomg.adventofcode.y2020.day2

object Day2 {
  def star1(data: List[PasswordValidation]): Int = {
    data.map(_.isValid).count(identity)
  }
}

case class PasswordValidation(password: String, rule: PasswordValidationRule) {
  def isValid: Boolean = rule.isValid(password)
}

case object PasswordValidation {
  def fromString(str: String): PasswordValidation = {
    val Array(rawRule: String, password: String, _*)    = str.split(": ")
    val Array(rawRange: String, rawChar: String, _*)    = rawRule.split(" ")
    val Array(rangeStart: String, rangeEnd: String, _*) = rawRange.split("-")
    val range                                           = Range.inclusive(rangeStart.toInt, rangeEnd.toInt)

    PasswordValidation(password, PasswordValidationRule(rawChar.charAt(0), range))
  }
}

case class PasswordValidationRule(char: Char, validRange: Range) {
  def isValid(check: String): Boolean = {
    val counter = check.foldLeft(0) { (acc, c) =>
      if (c == char) acc + 1 else acc
    }
    validRange.contains(counter)
  }
}
