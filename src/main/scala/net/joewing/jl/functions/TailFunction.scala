package net.joewing.jl.functions

import net.joewing.jl._
import net.joewing.jl.check._
import net.joewing.jl.interpret._

class TailFunction extends SpecialFunction {
  // (tail lst)

  def check(context: CheckerContext, args: List[Token]): (CheckerContext, TypeResult) = {
    if (args.length != 1) {
      (context, InvalidTypeResult("wrong number of arguments to tail"))
    } else {
      val (argContext, argType) = Checker.run(context, args.head)
      val containedType = UnknownTypeResult(new TypeId())
      val expectedType = ListTypeResult(containedType)
      (argContext.addEquivalence(argType, expectedType), argType)
    }
  }

  def run(context: InterpreterContext, args: List[Token]): (InterpreterContext, ValueResult) = {
    val (newContext, value) = Interpreter.run(context, args.head)
    value match {
      case ListValueResult(values) => (newContext, ListValueResult(values.tail))
      case _ => (newContext, NilValueResult())
    }
  }
}