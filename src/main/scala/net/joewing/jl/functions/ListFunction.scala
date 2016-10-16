package net.joewing.jl.functions

import net.joewing.jl._
import net.joewing.jl.check._
import net.joewing.jl.interpret._

class ListFunction extends SpecialFunction {
  // (list ...)

  def check(context: CheckerContext, args: List[Token]): (CheckerContext, TypeResult) = {
    val zero = (context, UnknownTypeResult(new TypeId())): (CheckerContext, TypeResult)
    val (argContext, argType) = args.foldLeft(zero) { (acc, token) =>
      val (oldContext, oldType) = acc
      val (newContext, newType) = Checker.run(oldContext, token)
      (newContext.addEquivalence(oldType, newType), newType)
    }
    (argContext, ListTypeResult(argType))
  }

  def run(context: InterpreterContext, args: List[Token]): (InterpreterContext, ValueResult) = {
    (context, ListValueResult(args.map(Interpreter.run(context, _)._2)))
  }
}
