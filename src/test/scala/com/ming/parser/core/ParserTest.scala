package com.ming.parser.core
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class ParserTest extends AnyFlatSpec with Matchers {
  "natural number parser" should "parse natural numbers" in {
    val actual = Parser.naturalNumber.run("123")
    val expected = Some("", 123)
    actual shouldBe expected
  }

  "natural number parser" should "parse natural numbers with string" in {
    val actual = Parser.naturalNumber.run("123abc")
    val expected = Some("abc", 123)
    actual shouldBe expected
  }

  "natural number parser" should "fail parsing only string" in {
    val actual = Parser.naturalNumber.run("abc")
    val expected = None
    actual shouldBe expected
  }
}
