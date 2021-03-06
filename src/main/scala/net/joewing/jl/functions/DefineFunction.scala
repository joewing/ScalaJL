package net.joewing.jl.functions

import net.joewing.jl._
import net.joewing.jl.check._
import net.joewing.jl.interpret._

class DefineFunction extends SpecialFunction {
  // (define name value...)

  def check(context: CheckerContext, expr: Token, args: List[Token]): (CheckerContext, TypeResult) =
    args match {
      case IdentToken(name) :: rest =>
        val tempType = UnknownTypeResult(expr, new TypeId())
        val tempContext = context.updateScope(Map(name -> tempType))
        val (resultContext, resultType) = Checker.run(tempContext, rest)
        (resultContext.addEquivalence(tempType, resultType), resultType)
      case _ => (context, InvalidTypeResult(expr, "too few arguments to define"))
    }

  def run(context: InterpreterContext, expr: Token, args: List[Token]): (InterpreterContext, ValueResult) =
    args match {
      case IdentToken(name) :: rest =>
        val (resultContext, resultValue) = Interpreter.run(context, rest)
        (resultContext.updateScope(Map(name -> resultValue)), resultValue)
      case _ => (context, NilValueResult())
    }
}
