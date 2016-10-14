package net.joewing.jl.functions

import net.joewing.jl._
import net.joewing.jl.check._
import net.joewing.jl.interpret._

class DefineFunction extends SpecialFunction {
  // (define name value...)

  def check(context: Context[TypeResult], args: List[Token]): (Context[TypeResult], TypeResult) =
    args match {
      case IdentToken(name) :: rest =>
        val (resultContext, resultType) = Checker.run(context, rest)
        val newContext = resultContext.updateScope(Map(name -> resultType))
        (newContext, resultType)
      case _ =>
        (context, InvalidTypeResult("invalid arguments to define"))
    }

  def run(context: Context[ValueResult], args: List[Token]): (Context[ValueResult], ValueResult) =
    args match {
      case IdentToken(name) :: rest =>
        val (resultContext, resultValue) = Interpreter.run(context, rest)
        val newContext = resultContext.updateScope(Map(name -> resultValue))
        (newContext, resultValue)
      case _ =>
        println("invalid arguments to define")
        (context, NilValueResult())
    }
}
