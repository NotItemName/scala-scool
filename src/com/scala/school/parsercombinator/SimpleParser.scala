package com.scala.school.parsercombinator

/**
 * @author Mykola Zalyayev
 */
trait SimpleParser[+T] extends Parser[T] {

  def acceptChar(expected: Char): Parser[Char] = new Parser[Char] {
    def apply(in: String) =
      OptionalString(in).getFirst match {
        case Some(first) => first.equals(expected) match {
          case true => Success(in.charAt(0), in.substring(1))
          case false => Failure("expected an " + expected, in)
        }
        case None => Failure("expected -> " + expected, in)
      }
  }

  def acceptString(EOS: Char): Parser[String] = new Parser[String] {
    def apply(in: String) = {
      val (left, right) = in.span(_ != EOS)
      if (!left.isEmpty) {
        Success(left, right)
      } else {
        Failure("expected string", in)
      }
    }
  }

  def acceptNumber: Parser[String] = new Parser[String] {
    val pattern = "[0-9]".r

    override def apply(in: String): Result[String] = {
      OptionalString(in).getFirst match {
        case Some(first) => pattern findFirstIn first.toString match {
          case Some(string) => apply(in.substring(1)).map(x => string + x)
          case None => Success("", in)
        }
        case None => Success("", in)
      }
    }
  }

  def acceptStartWith(str: String): Parser[String] = new Parser[String] {
    override def apply(in: String): Result[String] = {
      if (in.startsWith(str)) Success(str, in.replace(str, ""))
      else Failure("expected -> " + str, in)
    }
  }
}
