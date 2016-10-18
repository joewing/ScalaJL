package net.joewing.jl.functions

import net.joewing.jl._
import net.joewing.jl.check._
import net.joewing.jl.il.Program
import net.joewing.jl.interpret._

abstract class SpecialFunction {
  def check(context: CheckerContext, args: List[Token]): (CheckerContext, TypeResult)
  def run(context: InterpreterContext, args: List[Token]): (InterpreterContext, ValueResult)
  def generate: Program = ???
}

