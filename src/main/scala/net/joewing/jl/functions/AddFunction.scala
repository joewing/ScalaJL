package net.joewing.jl.functions

import net.joewing.jl._
import net.joewing.jl.check._
import net.joewing.jl.il.Program
import net.joewing.jl.interpret._

class AddFunction extends SpecialFunction {
  // (add ...)

  def check(context: CheckerContext, expr: Token, args: List[Token]): (CheckerContext, TypeResult) =
    context.fold(args)(IntegerTypeResult(expr): TypeResult) { (oldContext, oldType, token) =>
      val (newContext, newType) = Checker.run(oldContext, token)
      (oldType, newType) match {
        case (IntegerTypeResult(_), IntegerTypeResult(_)) => (newContext, IntegerTypeResult(expr))
        case (IntegerTypeResult(_), UnknownTypeResult(_, a)) =>
          (newContext.addBound(a, IntegerTypeResult(expr)), IntegerTypeResult(expr))
        case (left @ InvalidTypeResult(_, _), right @ InvalidTypeResult(_, _)) =>
          (newContext, InvalidTypeResult(left, right))
        case (invalid @ InvalidTypeResult(_, _), _) => (newContext, invalid)
        case (_, invalid @ InvalidTypeResult(_, _)) => (newContext, invalid)
        case _ => (newContext, InvalidTypeResult(expr, s"invalid types for add: $oldType vs $newType"))
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
