package test

import net.joewing.jl.Token
import net.joewing.jl.interpret._
import net.joewing.jl.parse._
import org.scalatest.FlatSpec

class InterpreterSpec extends FlatSpec {

  private[this] def getProgram(code: String): List[Token] = {
    ExpressionParser.parse(code).get
  }

  "print function" should "return nil" in {
    val program = getProgram("(print)")
    assert(Interpreter.run(program) == NilValueResult())
  }

  "define function" should "return value" in {
    val program = getProgram("(define name 5)")
    assert(Interpreter.run(program) == IntegerValueResult(5))
  }

  it should "assign a value" in {
    val program = getProgram("(define name 5) name")
    assert(Interpreter.run(program) == IntegerValueResult(5))
  }

  "add function" should "return 0" in {
    val program = getProgram("(add)")
    assert(Interpreter.run(program) == IntegerValueResult(0))
  }

  it should "add integers" in {
    val program = getProgram("(add 1 2 3)")
    assert(Interpreter.run(program) == IntegerValueResult(6))
  }

  "if function" should "handle true" in {
    val program = getProgram("(if true 1 2)")
    assert(Interpreter.run(program) == IntegerValueResult(1))
  }

  it should "handle false" in {
    val program = getProgram("(if false 1 2)")
    assert(Interpreter.run(program) == IntegerValueResult(2))
  }

  "lambda function" should "create a function" in {
    val program = getProgram("(define f (lambda () 5))(f)")
    assert(Interpreter.run(program) == IntegerValueResult(5))
  }

  it should "handle parameters" in {
    val program = getProgram("(define f (lambda (a) (add a a)))(f 3)")
    assert(Interpreter.run(program) == IntegerValueResult(6))
  }

  it should "handle recursion" in {
    val program = getProgram("(define f (lambda (i) (if (lt i 5) (add i (f (add i 1))) 0)))(f 0)")
    assert(Interpreter.run(program) == IntegerValueResult(10))
  }

  it should "use lexical scoping" in {
    val program = getProgram("(define a 3)(define b 6)(define f (lambda (b) (add a b)))(f 1)")
    assert(Interpreter.run(program) == IntegerValueResult(4))
  }

  it should "handle closures" in {
    val program = getProgram("(define f (lambda (a) (lambda (b) (add a b)))) ((f 1) 2)")
    assert(Interpreter.run(program) == IntegerValueResult(3))
  }

  "list function" should "create lists" in {
    val program = getProgram("(list 1 2)")
    assert(Interpreter.run(program) == ListValueResult(List(IntegerValueResult(1), IntegerValueResult(2))))
  }

}
