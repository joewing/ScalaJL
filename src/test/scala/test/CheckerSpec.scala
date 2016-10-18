package test

import net.joewing.jl.Token
import net.joewing.jl.check._
import net.joewing.jl.parse._
import org.scalatest.{FlatSpec, Matchers}

class CheckerSpec extends FlatSpec with Matchers {

  private[this] def checkProgram(code: String): TypeResult = {
    val program = new ExpressionParser("test").parse(code).get
    Checker.run(program)
  }

  "integer literals" should "be integers" in {
    val result = checkProgram("1")
    assert(result.isInstanceOf[IntegerTypeResult])
  }

  "boolean literals" should "be booleans" in {
    val result = checkProgram("true")
    assert(result.isInstanceOf[BooleanTypeResult])
  }

  "string literals" should "be strings" in {
    val result = checkProgram("\"test\"")
    assert(result.isInstanceOf[StringTypeResult])
  }

  "undefined values" should "be invalid" in {
    val result = checkProgram("test")
    assert(result.isInstanceOf[InvalidTypeResult])
  }

  "define function" should "return defined type" in {
    val result = checkProgram("(define a 1)")
    assert(result.isInstanceOf[IntegerTypeResult])
  }

  it should "set the type of a value" in {
    val result = checkProgram("(define a 1) a")
    assert(result.isInstanceOf[IntegerTypeResult])
  }

  it should "handle recursion" in {
    val result = checkProgram("(define f (lambda (i) (if (< i 5) (+ i (f (+ i 1))) 0)))(f 0)")
    assert(result.isInstanceOf[IntegerTypeResult])
  }

  "add function" should "return integers" in {
    val result = checkProgram("(+ 2 1)")
    assert(result.isInstanceOf[IntegerTypeResult])
  }

  it should "handle type errors" in {
    val result = checkProgram("(+ 2 \"asdf\")")
    assert(result.isInstanceOf[InvalidTypeResult])
  }

  it should "handle undefined values" in {
    val result = checkProgram("(+ 2 a)")
    assert(result.isInstanceOf[InvalidTypeResult])
  }

  "if function" should "return the right type" in {
    val result = checkProgram("(if true \"a\" \"b\")")
    assert(result.isInstanceOf[StringTypeResult])
  }

  it should "handle type mismatches" in {
    val result = checkProgram("(if true 1 \"b\")")
    assert(result.isInstanceOf[InvalidTypeResult])
  }

  it should "handle type invalid conditional" in {
    val result = checkProgram("(if 1 \"a\" \"b\")")
    assert(result.isInstanceOf[InvalidTypeResult])
  }

  it should "promote types" in {
    val result = checkProgram("(lambda (a) (if true a 0))")
    result should matchPattern {
      case LambdaTypeResult(_, List(IntegerTypeResult(_)), IntegerTypeResult(_)) =>
    }
  }

  "lambda function" should "return lambda type" in {
    val result = checkProgram("(lambda () 1)")
    result should matchPattern {
      case LambdaTypeResult(_, List(), IntegerTypeResult(_)) =>
    }
  }

  it should "handle unknown types" in {
    val result = checkProgram("(lambda (a) a)")
    result should matchPattern {
      case LambdaTypeResult(_, List(UnknownTypeResult(_, _)), UnknownTypeResult(_, _)) =>
    }
  }

  it should "promote types" in {
    val result = checkProgram("(lambda (a) (+ a 1))")
    result should matchPattern {
      case LambdaTypeResult(_, List(IntegerTypeResult(_)), IntegerTypeResult(_)) =>
    }
  }

  it should "handle calls" in {
    val result = checkProgram("(define f (lambda (a) (if a 1 2)))(f true)")
    assert(result.isInstanceOf[IntegerTypeResult])
  }

  it should "handle incorrect calls" in {
    val result = checkProgram("(define f (lambda (a) (if a 1 2)))(f 1)")
    assert(result.isInstanceOf[InvalidTypeResult])
  }

  "list function" should "create lists" in {
    val result = checkProgram("(list 1 2 3)")
    result should matchPattern {
      case ListTypeResult(_, IntegerTypeResult(_)) =>
    }
  }

  "head function" should "return the right type" in {
    val result = checkProgram("(head (list 1 2) 3)")
    assert(result.isInstanceOf[IntegerTypeResult])
  }

  "tail function" should "return a list" in {
    val result = checkProgram("(tail (list 1 2))")
    result should matchPattern {
      case ListTypeResult(_, IntegerTypeResult(_)) =>
    }
  }

  "empty? function" should "return a boolean" in {
    val result = checkProgram("(empty? (list))")
    assert(result.isInstanceOf[BooleanTypeResult])
  }

  it should "handle type errors" in {
    val result = checkProgram("(empty? 3)")
    assert(result.isInstanceOf[InvalidTypeResult])
  }

}
