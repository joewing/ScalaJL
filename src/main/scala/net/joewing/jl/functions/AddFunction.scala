package net.joewing.jl.functions

import net.joewing.jl._
import net.joewing.jl.check._
import net.joewing.jl.il.Program
import net.joewing.jl.interpret._

class AddFunction extends SpecialFunction {
  // (+ ...)

  def check(context: CheckerContext, expr: Token, args: List[Token]): (CheckerContext, TypeResult) = {
    val valueType = IntegerTypeResult(expr)
    context.fold(args)(valueType: TypeResult) { (oldContext, oldType, token) =>
      val (newContext, newType) = Checker.run(oldContext, token)
      val boundContext = newContext.addEquivalence(newType, valueType)
      (boundContext, valueType)
    }
  }

  def run(context: InterpreterContext, expr: Token, args: List[Token]): (InterpreterContext, ValueResult) =
    context.fold(args)(IntegerValueResult(0)) { (oldContext, oldValue, token) =>
      val (newContext, newValue) = Interpreter.run(oldContext, token)
      newValue match {
        case IntegerValueResult(i) => (newContext, IntegerValueResult(oldValue.value + i))
        case _ => (newContext, IntegerValueResult(0))
      }
    }

  def generate(args: List[Token]): Program = ???
}
