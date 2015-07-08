package com.scala.school.parsercombinator

/**
 * @author Mykola Zalyayev
 */
object Main {

  def main(args: Array[String]) {
    println(JsonParser( """ {"string":"value","number":123,"boolean":true,"object":{"val":"some"},"some":null,"array":[1,2,3]} """.trim))
  }

}
