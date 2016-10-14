package net.joewing.jl.check

import net.joewing.jl._
import net.joewing.jl.functions.Functions

object Checker extends Runner[TypeResult] {

  private val builtins: Map[String, SpecialTypeResult] = Functions() map { case (k, v) => k -> SpecialTypeResult(v) }

  private val baseScope = new Scope[TypeResult](0, -1, builtins)

  private val baseContext = new Context[TypeResult](List(), Map(0 -> baseScope), 0)

  override val nil: TypeResult = NilTypeResult()

  override def run(context: Context[TypeResult], token: Token): (Context[TypeResult], TypeResult) = {
    (context, NilTypeResult())
  }

}
