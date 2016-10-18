package net.joewing.jl.functions

import net.joewing.jl._
import net.joewing.jl.check._
import net.joewing.jl.il.Program
import net.joewing.jl.interpret._

class AddFunction extends SpecialFunction {
  // (add ...)

  def check(context: CheckerContext, args: List[Token]): (CheckerContext, TypeResult) =
    context.fold(args)(IntegerTypeResult(): TypeResult) { (oldContext, oldType, token) =>
      val (newContext, newType) = Checker.run(oldContext, token)
      (oldType, newType) match {
        case (IntegerTypeResult(), IntegerTypeResult()) => (newContext, IntegerTypeResult())
        case (IntegerTypeResult(), UnknownTypeResult(a)) =>
          (newContext.addBound(a, IntegerTypeResult()), IntegerTypeResult())
        case (InvalidTypeResult(msg), _) => (newContext, InvalidTypeResult(msg))
        case (_, InvalidTypeResult(msg)) => (newContext, InvalidTypeResult(msg))
        case _ => (newContext, InvalidTypeResult("invalid type for add"))
      }
    }

  def run(context: InterpreterContext, args: List[Token]): (InterpreterContext, ValueResult) =
    context.fold(args)(IntegerValueResult(0)) { (oldContext, oldValue, token) =>
      val (newContext, newValue) = Interpreter.run(oldContext, token)
      newValue match {
        case IntegerValueResult(i) => (newContext, IntegerValueResult(oldValue.value + i))
        case _ => (newContext, IntegerValueResult(0))
      }
    }

  def generate(args: List[Token]): Program = ???
}
