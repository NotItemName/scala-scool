package com.scala.school.parsercombinator
import ParserImplicits.stringToParser
/**
 * @author Mykola Zalyayev
 */
object JsonParser extends SimpleParser[JSONObj] {
  override def apply(in: String) = jsonObject apply in match {
    case Success(res, next) => Success(res, next)
  }


  def jsonObject = acceptChar('{') ~> repeat(jsonAttribute ~ jsonValue, ",") <~ acceptChar('}') >> (x => JSONObj(x))

  def jsonAttribute: Parser[JSONAttr] = acceptChar('"') ~> acceptString('"') <~ acceptChar('"') >> (x => JSONAttr(x))

  def jsonValue: Parser[JSONVal] = acceptChar(':') ~> values >> (x => JSONVal(x))

  def values: Parser[Any] = nullValue | booleanValue | stringValue | arrayValue | objectValue | numberValue

  def stringValue = acceptChar('"') ~> acceptString('"') <~ acceptChar('"')

  def objectValue: Parser[JSONObj] = acceptChar('{') ~> repeat(jsonAttribute ~ jsonValue, ",") <~ acceptChar('}') >> (x => JSONObj(x))

  def numberValue = acceptNumber

  def booleanValue = ("true" | "false") >> (x => x.toBoolean)

  def nullValue = "null"

  def arrayValue = acceptChar('[') ~> repeat(values, ",") <~ acceptChar(']')
}