package test

import net.joewing.jl.check._
import net.joewing.jl.parse._
import org.scalatest.FlatSpec

class CheckerSpec extends FlatSpec {

  "integer literals" should "be integers" in {
    val program = ExpressionParser.parse("1")
    assert(Checker.run(program.get) == IntegerTypeResult())
  }

  "boolean literals" should "be booleans" in {
    val program = ExpressionParser.parse("true")
    assert(Checker.run(program.get) == BooleanTypeResult())
  }

  "string literals" should "be strings" in {
    val program = ExpressionParser.parse("\"test\"")
    assert(Checker.run(program.get) == StringTypeResult())
  }

  "undefined values" should "be invalid" in {
    val program = ExpressionParser.parse("test")
    assert(Checker.run(program.get).isInstanceOf[InvalidTypeResult])
  }

  "define function" should "return defined type" in {
    val program = ExpressionParser.parse("(define a 1)")
    assert(Checker.run(program.get) == IntegerTypeResult())
  }

  it should "set the type of a value" in {
    val program = ExpressionParser.parse("(define a 1) a")
    assert(Checker.run(program.get) == IntegerTypeResult())
  }

  "add function" should "return integers" in {
    val program = ExpressionParser.parse("(add 2 1)")
    assert(Checker.run(program.get) == IntegerTypeResult())
  }

  it should "handle type errors" in {
    val program = ExpressionParser.parse("(add 2 \"asdf\")")
    assert(Checker.run(program.get).isInstanceOf[InvalidTypeResult])
  }

  it should "handle undefined values" in {
    val program = ExpressionParser.parse("(add 2 a)")
    assert(Checker.run(program.get).isInstanceOf[InvalidTypeResult])
  }

}
