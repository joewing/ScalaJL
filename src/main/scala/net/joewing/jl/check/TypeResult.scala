package net.joewing.jl.check

import net.joewing.jl._
import net.joewing.jl.functions.SpecialFunction

trait TypeResult
case class InvalidTypeResult(msg: String) extends TypeResult
case class UnknownTypeResult() extends TypeResult
case class NilTypeResult() extends TypeResult
case class BooleanTypeResult() extends TypeResult
case class IntegerTypeResult() extends TypeResult
case class StringTypeResult() extends TypeResult
case class SpecialTypeResult(func: SpecialFunction) extends TypeResult
case class LambdaTypeResult(
    scope: Int,
    args: List[TypeResult],
    ret: TypeResult)
  extends TypeResult
