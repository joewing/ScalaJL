package net.joewing.jl.functions

import net.joewing.jl._
import net.joewing.jl.check._
import net.joewing.jl.il.{ILContext, ValueId}
import net.joewing.jl.interpret._

abstract class SpecialFunction {
  def check(context: CheckerContext, expr: Token, args: List[Token]): (CheckerContext, TypeResult)
  def run(context: InterpreterContext, expr: Token, args: List[Token]): (InterpreterContext, ValueResult)
  def generate(context: ILContext, args: List[Token]): (ILContext, ValueId) = ???
}

