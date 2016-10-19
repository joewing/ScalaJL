package test

import net.joewing.jl.interpret._
import net.joewing.jl.parse._
import org.scalatest.FlatSpec

class InterpreterSpec extends FlatSpec {

  private[this] def runProgram(code: String): ValueResult = {
    val program = new ExpressionParser("test").parse(code).get
    Interpreter.run(program)
  }

  "print function" should "return nil" in {
    val result = runProgram("(print)")
    assert(result == NilValueResult())
  }

  "define function" should "return value" in {
    val result = runProgram("(define name 5)")
    assert(result == IntegerValueResult(5))
  }

  it should "assign a value" in {
    val result = runProgram("(define name 5) name")
    assert(result == IntegerValueResult(5))
  }

  "add function" should "return 0" in {
    val result = runProgram("(+)")
    assert(result == IntegerValueResult(0))
  }

  it should "add integers" in {
    val result = runProgram("(+ 1 2 3)")
    assert(result == IntegerValueResult(6))
  }

  "subtract function" should "subtract" in {
    val result = runProgram("(- 5 2)")
    assert(result == IntegerValueResult(3))
  }

  "if function" should "handle true" in {
    val result = runProgram("(if true 1 2)")
    assert(result == IntegerValueResult(1))
  }

  it should "handle false" in {
    val result = runProgram("(if false 1 2)")
    assert(result == IntegerValueResult(2))
  }

  "lambda function" should "create a function" in {
    val result = runProgram("(define f (lambda () 5))(f)")
    assert(result == IntegerValueResult(5))
  }

  it should "handle parameters" in {
    val result = runProgram("(define f (lambda (a) (+ a a)))(f 3)")
    assert(result == IntegerValueResult(6))
  }

  it should "handle recursion" in {
    val result = runProgram("(define f (lambda (i) (if (< i 5) (+ i (f (+ i 1))) 0)))(f 0)")
    assert(result == IntegerValueResult(10))
  }

  it should "use lexical scoping" in {
    val result = runProgram("(define a 3)(define b 6)(define f (lambda (b) (+ a b)))(f 1)")
    assert(result == IntegerValueResult(4))
  }

  it should "handle closures" in {
    val result = runProgram("(define f (lambda (a) (lambda (b) (+ a b)))) ((f 1) 2)")
    assert(result == IntegerValueResult(3))
  }

  it should "handle nested functions" in {
    val result = runProgram(
      "(define f (lambda (i) (+ i 1)))" +
      "(define s (lambda (i f) (if (< i 5) (+ (s (+ i 1) f) (f i)) 0)))" +
      "(s 0 f)"
    )
    assert(result == IntegerValueResult(15))
  }

  "list function" should "create lists" in {
    val result = runProgram("(list 1 2)")
    assert(result == ListValueResult(List(IntegerValueResult(1), IntegerValueResult(2))))
  }

  "head function" should "return the first element" in {
    val result = runProgram("(head (list 1 2 3) 4)")
    assert(result == IntegerValueResult(1))
  }

  it should "handle empty lists" in {
    val result = runProgram("(head (list) 1)")
    assert(result == IntegerValueResult(1))
  }

  "tail function" should "return the tail" in {
    val result = runProgram("(tail (list 1 2 3))")
    assert(result == ListValueResult(List(IntegerValueResult(2), IntegerValueResult(3))))
  }

  "empty? function" should "return true on empty lists" in {
    val result = runProgram("(empty? (list))")
    assert(result == BooleanValueResult(true))
  }

  it should "return false on non-empty lists" in {
    val result = runProgram("(empty? (list 1))")
    assert(result == BooleanValueResult(false))
  }

}
