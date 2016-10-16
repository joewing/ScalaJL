package net.joewing.jl.check

import net.joewing.jl._
import net.joewing.jl.functions.SpecialFunction

class TypeId

trait TypeResult {
  def solve(context: CheckerContext): TypeResult = this
}
case class InvalidTypeResult(msg: String) extends TypeResult
case class UnknownTypeResult(id: TypeId) extends TypeResult {
  override def solve(context: CheckerContext): TypeResult = context.solve(id)
}
case class NilTypeResult() extends TypeResult
case class BooleanTypeResult() extends TypeResult
case class IntegerTypeResult() extends TypeResult
case class StringTypeResult() extends TypeResult
case class SpecialTypeResult(func: SpecialFunction) extends TypeResult
case class LambdaTypeResult(args: List[TypeResult], ret: TypeResult) extends TypeResult {
  override def solve(context: CheckerContext): TypeResult = {
    LambdaTypeResult(args.map(_.solve(context)), ret.solve(context))
  }
}
