package gw

import scala.collection.mutable.ListBuffer

/**
  * Created by gavinwang on 23/11/16.
  */

class Parser(input: String) {
  private val inputReader = new InputReader(input)
  //current char, start from index 0
  private var char = inputReader.lookAhead()
  //represent the end of input
  private final val END = '\0'

  def parse(): Json = {
    val json = char match {
      case '{' => readObject()
      case '[' => readArray()
      case 't' => readBoolean()
      case 'f' => readBoolean()
      case 'n' => readNull()
      case c if c.isDigit => readNumber()
      case c if c.isWhitespace => {
        inputReader.skipWhiteSpace()
        char = inputReader.lookAhead()
        parse()
      }
      case '"' => readString()
      case _ => error()
    }
    json
  }

  def readObject(): JObject = {
    // char is '{'
    char = inputReader.nextChar()
    inputReader.skipWhiteSpace()
    if (inputReader.lookAhead() == '}') JObject(null)
    else {
      def readMap(map: Map[String, Json]): Map[String, Json] = {
        val key = readString()
        char = inputReader.nextChar()
        lookFor(':')
        char = inputReader.lookAhead()
        val value = parse()
        char = inputReader.nextChar()
        if (char != ',') map.updated(key.str, value)
        else readMap(map.updated(key.str, value))

      }
      val map = readMap(Map.empty[String, Json])
      lookFor('}')
      JObject(map)
    }
  }

  def readArray(): JArray = {
    //char is '['
    char = inputReader.nextChar()
    inputReader.skipWhiteSpace()
    if (inputReader.lookAhead() == ']') JArray(null)
    else {
      def readSeq(seq: ListBuffer[Json]): Seq[Json] = {
        char = inputReader.lookAhead()
        val value = parse()
        char = inputReader.nextChar()
        if (char != ',') seq += value
        else readSeq(seq += value)
      }
      val seq = readSeq(ListBuffer.empty[Json])
      lookFor(']')
      JArray(seq)
    }
  }

  def readNumber(): JNumber = {
    val stringBuilder = StringBuilder.newBuilder
    while (inputReader.lookAhead().isDigit) {
      char = inputReader.nextChar()
      stringBuilder.append(char)
    }
    JNumber(BigDecimal(stringBuilder.toString()))
  }

  def readNull(): Json = {
    val stringBuilder = StringBuilder.newBuilder
    (1 to 4).foreach { x =>
      if (inputReader.lookAhead() != END) {
        char = inputReader.nextChar()
        stringBuilder.append(char)
      }
    }
    if (stringBuilder.toString() == "null") JNull
    else error()
  }

  def readBoolean(): JBoolean = {
    val stringBuilder = StringBuilder.newBuilder
    //char is f or t
    char = inputReader.nextChar()
    stringBuilder.append(char)
    val times = if (char == 'f') 5 else 4
    (1 until times).foreach { x =>
      if (inputReader.lookAhead() != END) {
        char = inputReader.nextChar()
        stringBuilder.append(char)
      }
    }
    val booleanValue = stringBuilder.toString()
    if (booleanValue == "false") JBoolean(false)
    else if (booleanValue == "true") JBoolean(true)
    else error()
  }

  def readString(): JString = {
    val stringBuilder = StringBuilder.newBuilder
    //char is "
    char = inputReader.nextChar()
    char = inputReader.nextChar()
    while (char != END && char != '"') {
      stringBuilder.append(char)
      char = inputReader.nextChar()
    }
    lookFor('"')
    JString(stringBuilder.toString())
  }

  private def error() = throw new ParsingException(s"Invalid character:${char} at ${inputReader.at()}.")

  private def lookFor(c: Char): Unit = if (c != char) error()


}

case class ParsingException(msg: String) extends Exception


class InputReader(input: String) {
  //current Index
  private var index = -1
  private final val END = '\0'
  val length = if (input == null) 0 else input.length

  def nextChar(): Char = {
    index += 1
    if (index < length) input.charAt(index)
    else END
  }

  def at() = index

  def back() = index -= 1

  def lookAhead(): Char = {
    if (index + 1 < length) input.charAt(index + 1)
    else END
  }

  def skipWhiteSpace(): Unit = {
    if (lookAhead().isWhitespace) {
      nextChar()
      skipWhiteSpace()
    }
  }
}

