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
}