package test

import net.joewing.jl.check._
import net.joewing.jl.parse._
import org.scalatest.{FlatSpec, Matchers}

class CheckerSpec extends FlatSpec with Matchers {

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

  "if function" should "return the right type" in {
    val program = ExpressionParser.parse("(if true \"a\" \"b\")")
    assert(Checker.run(program.get) == StringTypeResult())
  }

  it should "handle type mismatches" in {
    val program = ExpressionParser.parse("(if true 1 \"b\")")
    assert(Checker.run(program.get).isInstanceOf[InvalidTypeResult])
  }

  it should "handle type invalid conditional" in {
    val program = ExpressionParser.parse("(if 1 \"a\" \"b\")")
    assert(Checker.run(program.get).isInstanceOf[InvalidTypeResult])
  }

  "lambda function" should "return lambda type" in {
    val program = ExpressionParser.parse("(lambda () 1)")
    Checker.run(program.get) should matchPattern {
      case LambdaTypeResult(_, List(), IntegerTypeResult()) =>
    }
  }

  it should "handle unknown types" in {
    val program = ExpressionParser.parse("(lambda (a) a)")
    Checker.run(program.get) should matchPattern {
      case LambdaTypeResult(_, List(UnknownTypeResult()), UnknownTypeResult()) =>
    }
  }

  it should "promote types" in {
    val program = ExpressionParser.parse("(lambda (a) (add a 1))")
    Checker.run(program.get) should matchPattern {
      case LambdaTypeResult(_, List(IntegerTypeResult()), IntegerTypeResult()) =>
    }
  }

}
