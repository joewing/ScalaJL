package test

import net.joewing.jl._
import net.joewing.jl.parse._
import org.scalatest.FlatSpec

class ExpressionParserSpec extends FlatSpec {

  val parser = ExpressionParser

  "The parser" should "parse a number" in {
    assert(parser.parse("53").get == List(IntegerToken(53)))
  }

  it should "parse an identifier" in {
    assert(parser.parse("asdf").get == List(IdentToken("asdf")))
  }

  it should "parse true" in {
    assert(parser.parse("true").get == List(BooleanToken(true)))
  }

  it should "parse false" in {
    assert(parser.parse("false").get == List(BooleanToken(false)))
  }

  it should "parse a string" in {
    assert(parser.parse("\"asdf\"").get == List(StringToken("asdf")))
  }

  it should "parse a simple expression" in {
    assert(parser.parse("(1 2)").get ==
      List(
        ExprToken(
          List(
            IntegerToken(1),
            IntegerToken(2)
          )
        )
      )
    )
  }

  it should "parse nested expressions" in {
    assert(parser.parse("(add (mult 3 four) 2)").get ==
      List(
        ExprToken(
          List(
            IdentToken("add"),
            ExprToken(
              List(
                IdentToken("mult"),
                IntegerToken(3),
                IdentToken("four")
              )
            ),
            IntegerToken(2)
          )
        )
      )
    )
  }
}
