package net.joewing.jl.interpret

import net.joewing.jl.functions.SpecialFunction
import net.joewing.jl._

trait ValueResult

case class NilValueResult() extends ValueResult {
  override def toString = "nil"
}

case class SpecialValueResult(func: SpecialFunction) extends ValueResult {
  override def toString = "<special>"
}

case class LambdaValueResult(
    stack: List[ScopeId],
    parameters: List[String],
    tokens: List[Token])
  extends ValueResult with LambdaResult {

  override def toString = "<lambda>"
}

case class BooleanValueResult(value: Boolean) extends ValueResult {
  override def toString = if (value) "true" else "false"
}

case class IntegerValueResult(value: Int) extends ValueResult {
  override def toString = value.toString
}

case class StringValueResult(value: String) extends ValueResult {
  override def toString = value
}
