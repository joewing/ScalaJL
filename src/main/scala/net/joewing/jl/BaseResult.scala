package net.joewing.jl

import net.joewing.jl.functions.SpecialFunction
import net.joewing.jl.interpret.{ValueResult, SpecialValueResult}
import net.joewing.jl.check.{TypeResult, SpecialTypeResult}

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
}