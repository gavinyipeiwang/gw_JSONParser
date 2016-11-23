package gw

/**
  * Created by gavinwang on 23/11/16.
  */
sealed abstract class Json {
}

case class JObject(kv: Map[String, Json]) extends Json {
  override def toString: String = "{" + kv.map(x => s"${x._1}:${x._2}").mkString(",") + "}"
}

case class JArray(objects: Seq[Json]) extends Json {
  override def toString: String = "[" + objects.map(_.toString).mkString(",") + "]"
}

case class JString(str: String) extends Json {
  override def toString: String = "\"" + s"${str}" + "\""
}

case class JNumber(num: BigDecimal) extends Json {
  override def toString: String = num.toString
}

object JNumber {

  def apply(num: Int) = new JNumber(BigDecimal(num))

  def apply(num: Long) = new JNumber(BigDecimal(num))

  def apply(num: Double) = new JNumber(BigDecimal(num))
}

case class JBoolean(boolean: Boolean) extends Json {
  override def toString: String = boolean.toString
}

case object JNull extends Json {
  override def toString: String = "null"
}

