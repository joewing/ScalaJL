package net.joewing.jl.functions

import net.joewing.jl._
import net.joewing.jl.check._
import net.joewing.jl.interpret._

class PrintFunction extends SpecialFunction {
  // (print ...)

  def check(context: CheckerContext, expr: Token, args: List[Token]): (CheckerContext, TypeResult) = {
    context.fold(args)(NilTypeResult(expr): TypeResult) { (oldContext, oldType, token) =>
      val (newContext, newType) = Checker.run(oldContext, token)
      (oldType, newType) match {
        case (left @ InvalidTypeResult(_, _), right @ InvalidTypeResult(_, _)) =>
          (newContext, InvalidTypeResult(left, right))
        case (left @ InvalidTypeResult(_, _), _) => (newContext, left)
        case (_, right @ InvalidTypeResult(_, _)) => (newContext, right)
        case _ => (newContext, newType)
      }
    }
  }

  def run(context: InterpreterContext, expr: Token, args: List[Token]): (InterpreterContext, ValueResult) = {
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
