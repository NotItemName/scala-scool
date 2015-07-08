package com.scala.school.parsercombinator

/**
 * @author Mykola Zalyayev
 */
object ParserImplicits {

  implicit def stringToParser(str: String):Parser[String] = new Parser[String] {
    override def apply(in: String): Result[String] = {
      if (in.startsWith(str)) Success(str, in.replace(str, ""))
      else Failure("expected -> " + str, in)
    }
  }

}
