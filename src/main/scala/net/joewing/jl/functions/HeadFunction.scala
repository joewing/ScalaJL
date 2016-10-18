package net.joewing.jl.functions

import net.joewing.jl._
import net.joewing.jl.check._
import net.joewing.jl.interpret._

class HeadFunction extends SpecialFunction {
  def check(context: CheckerContext, expr: Token, args: List[Token]): (CheckerContext, TypeResult) = {
    args match {
      case first :: second :: Nil =>
        val (listContext, listType) = Checker.run(context, first)
        val (argContext, containedType) = Checker.run(listContext, second)
        val expectedListType = ListTypeResult(second, containedType)
        (argContext.addEquivalence(listType, expectedListType), containedType)
      case _ => (context, InvalidTypeResult(expr, s"wrong number of arguments to head; got ${args.length}, expected 2"))
    }
  }

  def run(context: InterpreterContext, expr: Token, args: List[Token]): (InterpreterContext, ValueResult) = {
    assert(args.length == 2)
    val (newContext, listValue) = Interpreter.run(context, args.head)
    listValue match {
      case ListValueResult(values) if values.isEmpty => Interpreter.run(newContext, args(1))
      case ListValueResult(values) => (newContext, values.head)
      case _ => (newContext, NilValueResult())
    }
  }
}