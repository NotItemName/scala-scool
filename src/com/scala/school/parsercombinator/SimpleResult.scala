package com.scala.school.parsercombinator

/**
 * @author Mykola Zalyayev
 */
trait Result[+T] {
  def map[A](f:T => A): Result[A]

  def next: String
}

case class Success[+T](result: T, next: String) extends Result[T] {
  override def map[A](f: T => A): Result[A] = Success(f(result), next)
}

case class Failure(message: String, next: String) extends Result[Nothing] {
  override def map[A](f: (Nothing) => A): Result[Nothing] = Failure(message, next)
}
