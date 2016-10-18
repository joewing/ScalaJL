package net.joewing.jl.functions

import net.joewing.jl._
import net.joewing.jl.check._
import net.joewing.jl.interpret._

class IfFunction extends SpecialFunction {
  // (if cond true false)

  def check(context: CheckerContext, expr: Token, args: List[Token]): (CheckerContext, TypeResult) = {
    if (args.length != 3) {
      (context, InvalidTypeResult(expr, s"wrong number of arguments to if; got ${args.length}, expected 3"))
    }
    val (condContext, condType) = Checker.run(context, args.head)
    val (trueContext, trueType) = Checker.run(condContext, args(1))
    val (falseContext, falseType) = Checker.run(trueContext, args(2))
    (falseContext.addEquivalence(trueType, falseType).addEquivalence(condType, BooleanTypeResult(expr)), falseType)
  }

  def run(context: InterpreterContext, expr: Token, args: List[Token]): (InterpreterContext, ValueResult) = {
    assert(args.length == 3)
    Interpreter.run(context, args.head) match {
      case (newContext, BooleanValueResult(true)) => Interpreter.run(newContext, args(1))
      case (newContext, _) => Interpreter.run(newContext, args(2))
    }
  }
}
