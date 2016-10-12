
package test

import net.joewing.jl.interpret._
import net.joewing.jl.parse._
import org.scalatest.FlatSpec

class InterpreterSpec extends FlatSpec {

  "print function" should "return nil" in {
    val program = ExpressionParser.parse("(print)")
    assert(Interpreter.run(program.get) == NilValueResult())
  }

  "define function" should "return value" in {
    val program = ExpressionParser.parse("(define name 5)")
    assert(Interpreter.run(program.get) == IntegerValueResult(5))
  }

  it should "assign a value" in {
    val program = ExpressionParser.parse("(define name 5) name")
    assert(Interpreter.run(program.get) == IntegerValueResult(5))
  }

  "add function" should "return 0" in {
    val program = ExpressionParser.parse("(add)")
    assert(Interpreter.run(program.get) == IntegerValueResult(0))
  }

  it should "add integers" in {
    val program = ExpressionParser.parse("(add 1 2 3)")
    assert(Interpreter.run(program.get) == IntegerValueResult(6))
  }

  "if function" should "handle true" in {
    val program = ExpressionParser.parse("(if true 1 2)")
    assert(Interpreter.run(program.get) == IntegerValueResult(1))
  }

  it should "handle false" in {
    val program = ExpressionParser.parse("(if false 1 2)")
    assert(Interpreter.run(program.get) == IntegerValueResult(2))
  }

  "lambda function" should "create a function" in {
    val program = ExpressionParser.parse("(define f (lambda () 5))(f)")
    assert(Interpreter.run(program.get) == IntegerValueResult(5))
  }

  it should "handle parameters" in {
    val program = ExpressionParser.parse("(define f (lambda (a) (add a a)))(f 3)")
    assert(Interpreter.run(program.get) == IntegerValueResult(6))
  }

  it should "handle recursion" in {
    val program = ExpressionParser.parse("(define f (lambda (i) (if (lt i 5) (add i (f (add i 1))) 0)))(f 0)")
    assert(Interpreter.run(program.get) == IntegerValueResult(10))
  }

  it should "use lexical scoping" in {
    val program = ExpressionParser.parse("(define a 3)(define b 6)(define f (lambda (b) (add a b)))(f 1)")
    assert(Interpreter.run(program.get) == IntegerValueResult(4))
  }

  it should "handle closures" in {
    val program = ExpressionParser.parse("(define f (lambda (a) (lambda (b) (add a b)))) ((f 1) 2)")
    assert(Interpreter.run(program.get) == IntegerValueResult(3))
  }

}
