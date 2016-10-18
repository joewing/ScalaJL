package net.joewing.jl.functions

import net.joewing.jl._
import net.joewing.jl.check._
import net.joewing.jl.interpret._

class TailFunction extends SpecialFunction {
  // (tail lst)

  def check(context: CheckerContext, expr: Token, args: List[Token]): (CheckerContext, TypeResult) = {
    if (args.length != 1) {
      (context, InvalidTypeResult(expr, s"wrong number of arguments to tail; got ${args.length}, expected 1"))
    } else {
      val (argContext, argType) = Checker.run(context, args.head)
      val containedType = UnknownTypeResult(args.head, new TypeId())
      val expectedType = ListTypeResult(args.head, containedType)
      (argContext.addEquivalence(argType, expectedType), argType)
    }
  }

  def run(context: InterpreterContext, expr: Token, args: List[Token]): (InterpreterContext, ValueResult) = {
    val (newContext, value) = Interpreter.run(context, args.head)
    value match {
      case ListValueResult(values) => (newContext, ListValueResult(values.tail))
      case _ => (newContext, NilValueResult())
    }
  }
}