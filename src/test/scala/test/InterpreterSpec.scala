package test

import net.joewing.jl.Token
import net.joewing.jl.interpret._
import net.joewing.jl.parse._
import org.scalatest.FlatSpec

class InterpreterSpec extends FlatSpec {

  private[this] def getProgram(code: String): List[Token] = {
    new ExpressionParser("test").parse(code).get
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
    val program = getProgram("(+)")
    assert(Interpreter.run(program) == IntegerValueResult(0))
  }

  it should "add integers" in {
    val program = getProgram("(+ 1 2 3)")
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
    val program = getProgram("(define f (lambda (a) (+ a a)))(f 3)")
    assert(Interpreter.run(program) == IntegerValueResult(6))
  }

  it should "handle recursion" in {
    val program = getProgram("(define f (lambda (i) (if (< i 5) (+ i (f (+ i 1))) 0)))(f 0)")
    assert(Interpreter.run(program) == IntegerValueResult(10))
  }

  it should "use lexical scoping" in {
    val program = getProgram("(define a 3)(define b 6)(define f (lambda (b) (+ a b)))(f 1)")
    assert(Interpreter.run(program) == IntegerValueResult(4))
  }

  it should "handle closures" in {
    val program = getProgram("(define f (lambda (a) (lambda (b) (+ a b)))) ((f 1) 2)")
    assert(Interpreter.run(program) == IntegerValueResult(3))
  }

  it should "handle nested functions" in {
    val program = getProgram(
      "(define f (lambda (i) (+ i 1)))" +
      "(define s (lambda (i f) (if (< i 5) (+ (s (+ i 1) f) (f i)) 0)))" +
      "(s 0 f)"
    )
    assert(Interpreter.run(program) == IntegerValueResult(15))
  }

  "list function" should "create lists" in {
    val program = getProgram("(list 1 2)")
    assert(Interpreter.run(program) == ListValueResult(List(IntegerValueResult(1), IntegerValueResult(2))))
  }

  "head function" should "return the first element" in {
    val program = getProgram("(head (list 1 2 3) 4)")
    assert(Interpreter.run(program) == IntegerValueResult(1))
  }

  it should "handle empty lists" in {
    val program = getProgram("(head (list) 1)")
    assert(Interpreter.run(program) == IntegerValueResult(1))
  }

  "tail function" should "return the tail" in {
    val program = getProgram("(tail (list 1 2 3))")
    assert(Interpreter.run(program) == ListValueResult(List(IntegerValueResult(2), IntegerValueResult(3))))
  }

}
