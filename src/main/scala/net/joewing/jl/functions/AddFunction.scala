package net.joewing.jl.functions

import net.joewing.jl._
import net.joewing.jl.check._
import net.joewing.jl.interpret._

class AddFunction extends SpecialFunction {
  // (add ...)

  def check(context: CheckerContext, args: List[Token]): (CheckerContext, TypeResult) =
    args.foldLeft((context, IntegerTypeResult()): (CheckerContext, TypeResult)) { (acc, token) =>
      val (oldContext, oldType) = acc
      val (newContext, newType) = Checker.run(oldContext, token)
      (oldType, newType) match {
        case (IntegerTypeResult(), IntegerTypeResult()) => (newContext, IntegerTypeResult())
        case (IntegerTypeResult(), UnknownTypeResult()) => (newContext, IntegerTypeResult())
        case (InvalidTypeResult(msg), _) => (newContext, InvalidTypeResult(msg))
        case (_, InvalidTypeResult(msg)) => (newContext, InvalidTypeResult(msg))
        case _ => (newContext, InvalidTypeResult("invalid type for add"))
      }
    }

  def run(context: InterpreterContext, args: List[Token]): (InterpreterContext, ValueResult) = {
    args.foldLeft((context, IntegerValueResult(0))) { (acc, tok) =>
      val (oldContext, oldValue) = acc
      val (newContext, newValue) = Interpreter.run(oldContext, tok)
      newValue match {
        case IntegerValueResult(i) => (newContext, IntegerValueResult(oldValue.value + i))
        case _ =>
          println("invalid argument to add: " + tok)
          (newContext, IntegerValueResult(0))
      }
    }
  }
}
