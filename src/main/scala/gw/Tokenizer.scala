package gw

/**
  * Created by gavinwang on 23/11/16.
  */
object Tokenizer {

  def tokenize(input: String): Json = {
    val inputReader = new InputReader(input)
    while (inputReader.hasNext()) {
      val c = inputReader.nextChar()
      if (!c.isWhitespace) {
        c match {
          case '{' => readObject(inputReader)
          case '[' => readArray(inputReader)
          case _ => throw new ParsingException(s"Invalid character:${c}")
        }
      }
    }
  }

  private def readObject(inputReader: InputReader): JObject = ???

  private def readArray(inputReader: InputReader): JArray = ???

  def readString(inputReader: InputReader): JString = {
    val c = inputReader.nextChar()
    if (c != '"') throw new ParsingException(s"Invalid character:${c}")
    else {
      val str = inputReader.readTo('"')
      inputReader.nextChar()
      JString(str)
    }
  }

}

case class ParsingException(msg: String) extends Exception

class InputReader(input: String) {
  private var index = 0
  val length = if (input == null) 0 else input.length

  def nextChar(): Char = {
    val char = input.charAt(index)
    index += 1
    char
  }

  def hasNext(): Boolean = index < length

  def readTo(end: Char): String = {
    var isEnd = false
    val stringBuilder = StringBuilder.newBuilder
    while (!isEnd && hasNext()) {
      val char = nextChar()
      if (char == end) isEnd = true
      else stringBuilder.append(char)
    }
    if (!isEnd) throw new ParsingException("Expect \" ")
    else index -= 1
    stringBuilder.toString
  }

}

