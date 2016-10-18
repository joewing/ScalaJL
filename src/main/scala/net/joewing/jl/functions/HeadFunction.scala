package net.joewing.jl.functions

import net.joewing.jl._
import net.joewing.jl.check._
import net.joewing.jl.interpret._

class HeadFunction extends SpecialFunction {
  override def check(context: CheckerContext, args: List[Token]): (CheckerContext, TypeResult) = {
    args match {
      case first :: second :: Nil =>
        val (listContext, listType) = Checker.run(context, first)
        val (argContext, containedType) = Checker.run(listContext, second)
        val expectedListType = ListTypeResult(containedType)
        (argContext.addEquivalence(listType, expectedListType), containedType)
      case _ => (context, InvalidTypeResult("wrong number of arguments to head"))
    }
  }

  override def run(context: InterpreterContext, args: List[Token]): (InterpreterContext, ValueResult) = {
    assert(args.length == 2)
    val (newContext, listValue) = Interpreter.run(context, args.head)
    listValue match {
      case ListValueResult(values) if values.isEmpty => Interpreter.run(newContext, args(1))
      case ListValueResult(values) => (newContext, values.head)
      case _ => (newContext, NilValueResult())
    }
  }
}