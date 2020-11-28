package com.ming.parser.json

import com.ming.parser.core.Parser
import com.ming.parser.core.Parser._
import cats.implicits._

sealed trait JSON

object JSON {

  case object JNull extends JSON

  case class JNumber(get: Int) extends JSON

  case class JString(get: String) extends JSON

  case class JBoolean(get: Boolean) extends JSON

  case class JArray(get: IndexedSeq[JSON]) extends JSON

  case class JObject(get: Map[String, JSON]) extends JSON

}

object JsonParser {

  import JSON._

  val jsonNull: Parser[JSON] = string("null").map(_ => JNull)
  val jsonBoolean: Parser[JSON] = (string("true") <+> string("false")).map {
    case "true" => JBoolean(true)
    case "false" => JBoolean(false)
  }
  val jsonNumber: Parser[JSON] = naturalNumber.map(JNumber)
  val jsonString: Parser[JSON] = (char('"') *> literal <* char('"')).map(JString)

  val jsonValue = jsonNull <+> jsonBoolean <+> jsonNumber <+> jsonString
}
