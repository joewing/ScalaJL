package net.joewing.jl.functions

import net.joewing.jl._
import net.joewing.jl.check._
import net.joewing.jl.interpret._

class PrintFunction extends SpecialFunction {
  // (print ...)

  def check(context: Context[TypeResult], args: List[Token]): (Context[TypeResult], TypeResult) = {
    args.foldLeft((context, NilTypeResult()): (Context[TypeResult], TypeResult)) { (acc, token) =>
      val (oldContext, oldValue) = acc
      oldValue match {
        case InvalidTypeResult(_) => acc
        case _ => Checker.run(oldContext, token)
      }
    }
  }

  def run(context: Context[ValueResult], args: List[Token]): (Context[ValueResult], ValueResult) = {
    val result = args.foldLeft((context, NilValueResult()): (Context[ValueResult], ValueResult)) { (acc, token) =>
      val (oldContext, _) = acc
      val (newContext, newValue) = Interpreter.run(oldContext, token)
      print(newValue)
      (newContext, newValue)
    }
    println()
    result
  }
}
