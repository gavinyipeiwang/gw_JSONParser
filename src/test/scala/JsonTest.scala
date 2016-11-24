/**
  * Created by gavinwang on 23/11/16.
  */

import gw._
import org.scalatest._

class JsonTest extends FunSuite {
  test("JNull should be printed as null") {
    assert(JNull.toString == "null")
  }
  test("JBoolean(true) should be printed as true") {
    assert(JBoolean(true).toString == "true")
  }
  test("JBoolean(false) should be printed as false") {
    assert(JBoolean(false).toString == "false")
  }
  test("JNumber(2) should be printed as 2") {
    assert(JNumber(2).toString == "2")
  }
  test("JNumber(2.3) should be printed as 2.3") {
    assert(JNumber(2.3).toString == "2.3")
  }
  test("JNumber(123L) should be printed as 123") {
    assert(JNumber(123L).toString == "123")
  }
  test("JNumber(-1E1) should be printed as -10.0") {
    assert(JNumber(-1E1).toString == "-10.0")
  }
  test("JString(\"test\") should be printed as \"test\"") {
    assert(JString("test").toString == "\"test\"")
  }
  test("JArray(Seq(JNumber(123L),JBoolean(false))) should be printed as [123,false]") {
    assert(JArray(Seq(JNumber(123L), JBoolean(false))).toString == "[123,false]")
  }
  test("JObject(Map(\"k1\" -> JString(\"test\"), \"k2\" -> JArray(Seq(JNumber(123L), JBoolean(false))))) should be printed as {k1:\"test\",k2:[123,false]}") {
    assert(JObject(Map("k1" -> JString("test"), "k2" -> JArray(Seq(JNumber(123L), JBoolean(false))))).toString == "{k1:\"test\",k2:[123,false]}")
  }

  test("InputReader(\"abc\") has next.") {
    val inputReader = new InputReader("abc")
    assert(inputReader.hasNext())
  }
  test("InputReader(\"\") or InputReader(null) does not have next.") {
    val inputReader1 = new InputReader("")
    val inputReader2 = new InputReader(null)
    assert(!inputReader1.hasNext())
    assert(!inputReader2.hasNext())
  }
  test("InputReader(\"abc\") nextChar() nextChar() is b") {
    val inputReader = new InputReader("abc")
    inputReader.nextChar()
    val char = inputReader.nextChar()
    assert(char == 'b')
  }
  test("InputReader(\"abc\") read to 'b' is ab") {
    val inputReader = new InputReader("abc")
    val str = inputReader.readTo('c')
    assert(str == "ab")
    assert(inputReader.nextChar() == 'c')
  }
  test("InputReader(\"abc\") read to 'e' should throw a ParsingException") {
    val inputReader = new InputReader("abc")
    intercept[ParsingException] {
      inputReader.readTo('e')
    }
  }
}
