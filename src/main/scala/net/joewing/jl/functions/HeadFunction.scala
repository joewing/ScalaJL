package net.joewing.jl.functions

import net.joewing.jl._
import net.joewing.jl.check._
import net.joewing.jl.interpret._

class HeadFunction extends SpecialFunction {
  override def check(context: CheckerContext, args: List[Token]): (CheckerContext, TypeResult) = {
    if (args.length != 2) {
      (context, InvalidTypeResult("too few arguments to head"))
    } else {
      val (listContext, listType) = Checker.run(context, args.head)
      val (argContext, containedType) = Checker.run(listContext, args(1))
      val expectedListType = ListTypeResult(containedType)
      val newContext = argContext.addEquivalence(listType, expectedListType)
      (newContext, containedType)
    }
  }

  override def run(context: InterpreterContext, args: List[Token]): (InterpreterContext, ValueResult) = {
    assert(args.length == 2)
    val (_, listValue) = Interpreter.run(context, args.head)
    listValue match {
      case ListValueResult(values) =>
        if (values.isEmpty) {
          Interpreter.run(context, args(1))
        } else {
          (context, values.head)
        }
      case _ => (context, NilValueResult())
    }
  }
}