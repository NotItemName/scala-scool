package com.scala.school.parsercombinator

/**
 * @author Mykola Zalyayev
 */
case class OptionalString(string: String) {
  def getFirst: Option[Char] = {
    if (string == null || string.length == 0) {
      return None
    }
    Option(string.charAt(0))
  }
}
