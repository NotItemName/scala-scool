package com.scala.school.parsercombinator

/**
 * @author Mykola Zalyayev
 */
case class JSONObj(attributes: List[(JSONAttr, JSONVal)])

case class JSONAttr(name: String)

case class JSONVal(value: Any)
