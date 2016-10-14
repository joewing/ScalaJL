package net.joewing.jl.functions

import net.joewing.jl._
import net.joewing.jl.check._
import net.joewing.jl.interpret._

class IfFunction extends SpecialFunction {
  // (if cond true false)

  def check(context: Context[TypeResult], args: List[Token]): (Context[TypeResult], TypeResult) = {
    if (args.length != 3) {
      (context, InvalidTypeResult("wrong number of arguments to if"))
    }
    val (condContext, condType) = Checker.run(context, args.head)
    val (trueContext, trueType) = Checker.run(condContext, args(1))
    val (falseContext, falseType) = Checker.run(trueContext, args(2))
    (condType, trueType, falseType) match {
      case (InvalidTypeResult(_), _, _) => (falseContext, condType)
      case (_, InvalidTypeResult(_), _) => (falseContext, trueType)
      case (_, _, InvalidTypeResult(_)) => (falseContext, falseType)
      case (BooleanTypeResult(), _, _) if trueType == falseType => (falseContext, trueType)
      case _ => (falseContext, InvalidTypeResult("type mismatch for if"))
    }
  }

  def run(context: Context[ValueResult], args: List[Token]): (Context[ValueResult], ValueResult) = {
    assert(args.length == 3)
    Interpreter.run(context, args.head) match {
      case (newContext, BooleanValueResult(true)) => Interpreter.run(newContext, args(1))
      case (newContext, _) => Interpreter.run(newContext, args(2))
    }
  }
}
