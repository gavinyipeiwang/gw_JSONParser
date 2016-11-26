package gw

/**
  * Created by gavinwang on 23/11/16.
  */

class Parser(input: String) {
  private val inputReader = new InputReader(input)
  private var char = if (inputReader.hasNext()) inputReader.nextChar() else throw new ParsingException("Empty input.")

  def parse(): Json = {
    char match {
      case '{' => readObject()
      case '[' => readArray()
      case 't' => readBoolean()
      case 'f' => readBoolean()
      case 'n' => readNull()
      case c if char.isDigit => readNumber()
      case '"' => readString()
      case _ => error()
    }
  }

  def readObject(): JObject = {
    if (char != '{') error()
    else if (inputReader.hasNext()) {
      char = inputReader.nextChar()
      if (char == '}') JObject(null)
      else {
        def readMap(map: Map[String, Json]): Map[String, Json] = {
          val key = readString()
          if (inputReader.hasNext()) {
            char = inputReader.nextChar()
            if (char == ':') {
              char = inputReader.nextChar()
              val value = parse()
              char = inputReader.nextChar()
              if (char != ',') map.updated(key.str, value)
              else {
                char = inputReader.nextChar()
                readMap(map.updated(key.str, value))
              }
            } else error()
          } else error()
        }
        val map = readMap(Map.empty[String, Json])
        if (char == '}') JObject(map)
        else error()
      }
    } else error()
  }

  def readArray(): JArray = ???

  def readNumber(): JNumber = {
    if (!char.isDigit) error()
    else {
      val stringBuilder = StringBuilder.newBuilder
      stringBuilder.append(char)
      while (inputReader.hasNext() && char.isDigit) {
        char = inputReader.nextChar()
        stringBuilder.append(char)
      }
      if (!stringBuilder.charAt(stringBuilder.length - 1).isDigit) {
        inputReader.back()
        stringBuilder.deleteCharAt(stringBuilder.length - 1)
      }
      JNumber(BigDecimal(stringBuilder.toString()))
    }
  }

  def readNull(): Json = {
    if (char != 'n') error()
    else {
      val stringBuilder = StringBuilder.newBuilder
      stringBuilder.append(char)
      (1 until 4).foreach { x =>
        if (inputReader.hasNext()) {
          stringBuilder.append(inputReader.nextChar())
        }
      }
      val nullValue = stringBuilder.toString()
      if (nullValue == "null") JNull
      else error()
    }
  }

  def readBoolean(): JBoolean = {
    if (char != 'f' && char != 't') error()
    else {
      val stringBuilder = StringBuilder.newBuilder
      stringBuilder.append(char)
      val times = if (char == 'f') 5 else 4
      (1 until times).foreach { x =>
        if (inputReader.hasNext()) {
          stringBuilder.append(inputReader.nextChar())
        }
      }
      val booleanValue = stringBuilder.toString()
      if (booleanValue == "false") JBoolean(false)
      else if (booleanValue == "true") JBoolean(true)
      else error()
    }
  }

  def readString(): JString = {
    if (char != '"') error()
    else if (inputReader.hasNext()) {
      val stringBuilder = StringBuilder.newBuilder
      do {
        char = inputReader.nextChar()
        if (char != '"') {
          stringBuilder.append(char)
        }
      } while (inputReader.hasNext() && char != '"')
      if (char != '"') error()
      JString(stringBuilder.toString())
    } else error()
  }

  private def error() = throw new ParsingException(s"Invalid character:${char} at ${inputReader.at()}.")


}

case class ParsingException(msg: String) extends Exception

class InputReader(input: String) {
  //current Index
  private var index = -1
  val length = if (input == null) 0 else input.length

  def nextChar(): Char = {
    index += 1
    if (index < length) input.charAt(index)
    else throw new StringIndexOutOfBoundsException(s"(${index})")
  }

  def hasNext(): Boolean = index + 1 < length

  def at() = index

  def back() = index -= 1
}

