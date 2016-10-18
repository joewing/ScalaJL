package test

import net.joewing.jl._
import net.joewing.jl.parse._
import org.scalatest.FlatSpec

class ExpressionParserSpec extends FlatSpec {

  val parser = new ExpressionParser("test")

  "The parser" should "parse a number" in {
    assert(parser.parse("53").get == List(IntegerToken(53)))
  }

  it should "handle newlines" in {
    assert(parser.parse("asdf\n").get == List(IdentToken("asdf")))
  }

  it should "parse negative numbers" in {
    assert(parser.parse("-5").get == List(IntegerToken(-5)))
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

  it should "parse symbols" in {
    assert(parser.parse("< > <= ? !").get == List(
      IdentToken("<"),
      IdentToken(">"),
      IdentToken("<="),
      IdentToken("?"),
      IdentToken("!")
    ))
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
    assert(parser.parse("(+ (* 3 four) 2)").get ==
      List(
        ExprToken(
          List(
            IdentToken("+"),
            ExprToken(
              List(
                IdentToken("*"),
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
