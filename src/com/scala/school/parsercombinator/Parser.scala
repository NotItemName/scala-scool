package com.scala.school.parsercombinator

/**
 * @author Mykola Zalyayev
 */
trait Parser[+T] extends (String => Result[T]) {
  def apply(in: String): Result[T]

  def ~[U](p: => Parser[U]): Parser[(T, U)] = new Parser[(T, U)] {
    override def apply(in: String): Result[(T, U)] = Parser.this(in) match {
      case Success(x, next) => p(next) match {
        case Success(x2, next2) => Success((x, x2), next2)
        case Failure(m, n) => Failure(m, n);
      }
      case Failure(m, n) => Failure(m, n)
    }
  }

  def ~>[U](p: => Parser[U]): Parser[U] = new Parser[U] {
    override def apply(in: String) =
      Parser.this(in) match {
        case Success(x, next) => p(next) match {
          case Success(x2, next2) => Success(x2, next2)
          case Failure(m, n) => Failure(m, n);
        }
        case Failure(m, n) => Failure(m, n)
      }
  }

  def <~[U](p: => Parser[U]): Parser[T] = new Parser[T] {
    override def apply(in: String) =
      Parser.this(in) match {
        case Success(x, next) => p(next) match {
          case Success(x2, next2) => Success(x, next2)
          case Failure(m, n) => Failure(m, n);
        }
        case Failure(m, n) => Failure(m, n)
      }
  }

  def >>[B](f: T => B): Parser[B] = new Parser[B] {
    override def apply(in: String) =
      Parser.this(in) match {
        case Success(x, next) => Success(f(x), next)
        case Failure(m, n) => Failure(m, n)
      }
  }

  def |[U >:T](p: => Parser[U]): Parser[U] = new Parser[U] {
    override def apply(in: String): Result[U] =
      Parser.this(in) match {
        case Success(x, next) => Success(x, next)
        case Failure(m, n) => p(in)
      }
  }

  def repeat[T](p: => Parser[T], splitter: String): Parser[List[T]] = new Parser[List[T]] {
    override def apply(in: String): Result[List[T]] = p(in) match {
      case Success(elem, rest) => apply(rest).map(x => elem :: x)
      case _ if in.startsWith(splitter) => apply(in.drop(splitter.length))
      case _ => Success(Nil, in)
    }
  }
}