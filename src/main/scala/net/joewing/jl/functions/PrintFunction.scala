package net.joewing.jl.functions

import net.joewing.jl._
import net.joewing.jl.check._
import net.joewing.jl.interpret._

class PrintFunction extends SpecialFunction {
  // (print ...)

  def check(context: CheckerContext, args: List[Token]): (CheckerContext, TypeResult) = {
    context.fold(args)(NilTypeResult(): TypeResult) { (oldContext, oldType, token) =>
      oldType match {
        case invalid: InvalidTypeResult => (oldContext, oldType)
        case _ => Checker.run(oldContext, token)
      }
    }
  }

  def run(context: InterpreterContext, args: List[Token]): (InterpreterContext, ValueResult) = {
    val result = args.foldLeft((context, NilValueResult()): (InterpreterContext, ValueResult)) { (acc, token) =>
      val (oldContext, _) = acc
      val (newContext, newValue) = Interpreter.run(oldContext, token)
      print(newValue)
      (newContext, newValue)
    }
    println()
    result
  }
}
