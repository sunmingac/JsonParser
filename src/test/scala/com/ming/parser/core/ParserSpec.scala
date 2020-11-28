package com.ming.parser.core

import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec
import Parser._

class ParserSpec extends AnyWordSpec with Matchers {
  "natural number parser" should {
    "parse numbers" in {
      val actual = naturalNumber.run("123")
      val expected = Some("", 123)
      actual shouldBe expected
    }
    "parse numbers with string" in {
      val actual = naturalNumber.run("123abc")
      val expected = Some("abc", 123)
      actual shouldBe expected
    }
    "fail parsing string" in {
      val actual = naturalNumber.run("abc")
      val expected = None
      actual shouldBe expected
    }
    "fail parsing empty string" in {
      val actual = naturalNumber.run("")
      val expected = None
      actual shouldBe expected
    }
  }

  "literal parser" should {
    "parse string" in {
      val actual = literal.run("abc")
      val expected = Some("", "abc")
      actual shouldBe expected
    }
    "fail parsing double quote symbol" in {
      val actual = literal.run("a\"bc")
      val expected = None
      actual shouldBe expected
    }
  }

}
