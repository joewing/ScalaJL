package net.joewing.jl.functions

import net.joewing.jl._
import net.joewing.jl.check._
import net.joewing.jl.interpret._

class ListFunction extends SpecialFunction {
  // (list ...)

  def check(context: CheckerContext, expr: Token, args: List[Token]): (CheckerContext, TypeResult) = {
    val result = context.fold(args)(UnknownTypeResult(expr, new TypeId()): TypeResult) { (oldContext, oldType, token) =>
      val (newContext, newType) = Checker.run(oldContext, token)
      (newContext.addEquivalence(oldType, newType), newType)
    }
    (result._1, ListTypeResult(expr, result._2))
  }

  def run(context: InterpreterContext, expr: Token, args: List[Token]): (InterpreterContext, ValueResult) = {
    val result = context.map(args)(Interpreter.run)
    (result._1, ListValueResult(result._2))
  }
}
