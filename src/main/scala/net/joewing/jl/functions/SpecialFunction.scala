package net.joewing.jl.functions

import net.joewing.jl._
import net.joewing.jl.check._
import net.joewing.jl.interpret._

abstract class SpecialFunction {
  def check(context: Context[TypeResult], args: List[Token]): (Context[TypeResult], TypeResult)
  def run(context: Context[ValueResult], args: List[Token]): (Context[ValueResult], ValueResult)
}

