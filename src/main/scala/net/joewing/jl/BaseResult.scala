package net.joewing.jl

import net.joewing.jl.functions.SpecialFunction
import net.joewing.jl.interpret.{SpecialValueResult, ValueResult}
import net.joewing.jl.check.{SpecialTypeResult, TypeResult}
import net.joewing.jl.il._

trait BaseResult[T] {
  def create(func: SpecialFunction): T
}

object BaseResult {
  implicit object svr extends BaseResult[ValueResult] {
    def create(func: SpecialFunction): ValueResult = SpecialValueResult(func)
  }
  implicit object str extends BaseResult[TypeResult] {
    def create(func: SpecialFunction): TypeResult = SpecialTypeResult(InvalidToken(), func)
  }
  implicit object spr extends BaseResult[Program] {
    def create(func: SpecialFunction): Program = Program(new InstructionId, List(), Map(), Map())
  }
}