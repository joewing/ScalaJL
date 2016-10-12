package net.joewing.jl.check

import net.joewing.jl._
import net.joewing.jl.functions.Functions

object Checker extends Runner[TypeResult, CheckerContext] {

  private val builtins: Map[String, SpecialTypeResult] = Functions() map { case (k, v) => k -> SpecialTypeResult(v) }

  private val baseScope = Scope[TypeResult, CheckerContext](0, -1, builtins)

  private val baseContext = new CheckerContext(Map(0 -> baseScope), 0)

  override val nil: TypeResult = NilTypeResult()

  override def run(context: CheckerContext, token: Token): (CheckerContext, TypeResult) = {
    (context, NilTypeResult())
  }

}
