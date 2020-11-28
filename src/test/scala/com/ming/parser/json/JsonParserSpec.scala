package com.ming.parser.json

import com.ming.parser.json.JSON.{JBoolean, JNull, JNumber, JString}
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class JsonParserSpec extends AnyWordSpec with Matchers {
  "Json null parser" should {
    "parse null" in {
      val actual = JsonParser.jsonNull.run("null")
      val expected = Some("", JNull)
      actual shouldBe expected
    }
    "fail parsing invalid string" in {
      val actual = JsonParser.jsonNull.run("abc")
      val expected = None
      actual shouldBe expected
    }
  }

  "Json boolean parser" should {
    "parse true" in {
      val actual = JsonParser.jsonBoolean.run("true")
      val expected = Some("", JBoolean(true))
      actual shouldBe expected
    }
    "parse false" in {
      val actual = JsonParser.jsonBoolean.run("false")
      val expected = Some("", JBoolean(false))
      actual shouldBe expected
    }
    "fail parsing invalid boolean" in {
      val actual = JsonParser.jsonBoolean.run("abc")
      val expected = None
      actual shouldBe expected
    }
  }

  "Json number parser" should {
    "parse integer" in {
      val actual = JsonParser.jsonNumber.run("123")
      val expected = Some("", JNumber(123))
      actual shouldBe expected
    }
    "fail parsing non integer" in {
      val actual = JsonParser.jsonNumber.run("abc")
      val expected = None
      actual shouldBe expected
    }
  }

  "Json string parser" should {
    "parse string within double quotes" in {
      val actual = JsonParser.jsonString.run("\"abc\"")
      val expected = Some("", JString("abc"))
      actual shouldBe expected
    }
    "fail parsing without double quotes" in {
      val actual = JsonParser.jsonString.run("abc")
      val expected = None
      actual shouldBe expected
    }
  }
}
