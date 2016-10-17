package test

import net.joewing.jl.Token
import net.joewing.jl.check._
import net.joewing.jl.parse._
import org.scalatest.{FlatSpec, Matchers}

class CheckerSpec extends FlatSpec with Matchers {

  private[this] def getProgram(code: String): List[Token] = ExpressionParser.parse(code).get

  "integer literals" should "be integers" in {
    val program = getProgram("1")
    assert(Checker.run(program) == IntegerTypeResult())
  }

  "boolean literals" should "be booleans" in {
    val program = getProgram("true")
    assert(Checker.run(program) == BooleanTypeResult())
  }

  "string literals" should "be strings" in {
    val program = getProgram("\"test\"")
    assert(Checker.run(program) == StringTypeResult())
  }

  "undefined values" should "be invalid" in {
    val program = getProgram("test")
    assert(Checker.run(program).isInstanceOf[InvalidTypeResult])
  }

  "define function" should "return defined type" in {
    val program = getProgram("(define a 1)")
    assert(Checker.run(program) == IntegerTypeResult())
  }

  it should "set the type of a value" in {
    val program = getProgram("(define a 1) a")
    assert(Checker.run(program) == IntegerTypeResult())
  }

  "add function" should "return integers" in {
    val program = getProgram("(add 2 1)")
    assert(Checker.run(program) == IntegerTypeResult())
  }

  it should "handle type errors" in {
    val program = getProgram("(add 2 \"asdf\")")
    assert(Checker.run(program).isInstanceOf[InvalidTypeResult])
  }

  it should "handle undefined values" in {
    val program = getProgram("(add 2 a)")
    assert(Checker.run(program).isInstanceOf[InvalidTypeResult])
  }

  "if function" should "return the right type" in {
    val program = getProgram("(if true \"a\" \"b\")")
    assert(Checker.run(program) == StringTypeResult())
  }

  it should "handle type mismatches" in {
    val program = getProgram("(if true 1 \"b\")")
    assert(Checker.run(program).isInstanceOf[InvalidTypeResult])
  }

  it should "handle type invalid conditional" in {
    val program = getProgram("(if 1 \"a\" \"b\")")
    assert(Checker.run(program).isInstanceOf[InvalidTypeResult])
  }

  it should "promote types" in {
    val program = getProgram("(lambda (a) (if true a 0))")
    Checker.run(program) should matchPattern {
      case LambdaTypeResult(List(IntegerTypeResult()), IntegerTypeResult()) =>
    }
  }

  "lambda function" should "return lambda type" in {
    val program = getProgram("(lambda () 1)")
    Checker.run(program) should matchPattern {
      case LambdaTypeResult(List(), IntegerTypeResult()) =>
    }
  }

  it should "handle unknown types" in {
    val program = getProgram("(lambda (a) a)")
    Checker.run(program) should matchPattern {
      case LambdaTypeResult(List(UnknownTypeResult(_)), UnknownTypeResult(_)) =>
    }
  }

  it should "promote types" in {
    val program = getProgram("(lambda (a) (add a 1))")
    Checker.run(program) should matchPattern {
      case LambdaTypeResult(List(IntegerTypeResult()), IntegerTypeResult()) =>
    }
  }

  it should "handle calls" in {
    val program = getProgram("(define f (lambda (a) (if a 1 2)))(f true)")
    assert(Checker.run(program).isInstanceOf[IntegerTypeResult])
  }

  it should "handle incorrect calls" in {
    val program = getProgram("(define f (lambda (a) (if a 1 2)))(f 1)")
    assert(Checker.run(program).isInstanceOf[InvalidTypeResult])
  }

  "list function" should "create lists" in {
    val program = getProgram("(list 1 2 3)")
    Checker.run(program) should matchPattern {
      case ListTypeResult(IntegerTypeResult()) =>
    }
  }

  "head function" should "return the right type" in {
    val program = getProgram("(head (list 1 2) 3)")
    assert(Checker.run(program).isInstanceOf[IntegerTypeResult])
  }

}
