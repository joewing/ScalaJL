package net.joewing.jl.functions

import net.joewing.jl._
import net.joewing.jl.check._
import net.joewing.jl.interpret._

class IfFunction extends SpecialFunction {
  // (if cond true false)

  def check(context: CheckerContext, args: List[Token]): (CheckerContext, TypeResult) = {
    if (args.length != 3) {
      (context, InvalidTypeResult("wrong number of arguments to if"))
    }
    val (condContext, condType) = Checker.run(context, args.head)
    val (trueContext, trueType) = Checker.run(condContext, args(1))
    val (falseContext, falseType) = Checker.run(trueContext, args(2))
    val newContext = falseContext.addEquivalence(trueType, falseType)
    (condType, trueType, falseType) match {
      case (InvalidTypeResult(_), _, _) => (newContext, condType)
      case (_, InvalidTypeResult(_), _) => (newContext, trueType)
      case (_, _, InvalidTypeResult(_)) => (newContext, falseType)
      case (BooleanTypeResult(), _, _) if trueType == falseType => (newContext, trueType)
      case _ => (newContext, InvalidTypeResult("type mismatch for if"))
    }
  }

  def run(context: InterpreterContext, args: List[Token]): (InterpreterContext, ValueResult) = {
    assert(args.length == 3)
    Interpreter.run(context, args.head) match {
      case (newContext, BooleanValueResult(true)) => Interpreter.run(newContext, args(1))
      case (newContext, _) => Interpreter.run(newContext, args(2))
    }
  }
}
