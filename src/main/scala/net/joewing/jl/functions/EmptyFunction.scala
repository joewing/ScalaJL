package net.joewing.jl.functions
import net.joewing.jl.Token
import net.joewing.jl.check._
import net.joewing.jl.interpret._

class EmptyFunction extends SpecialFunction {
  def check(context: CheckerContext, expr: Token, args: List[Token]): (CheckerContext, TypeResult) = {
    if (args.length != 1) {
      (context, InvalidTypeResult(expr, s"wrong number of arguments to empty?; got ${args.length}, expected 1"))
    } else {
      val containedType = UnknownTypeResult(expr, new TypeId())
      val listType = ListTypeResult(expr, containedType)
      val (argContext, argType) = Checker.run(context, args.head)
      (argContext.addEquivalence(listType, argType), BooleanTypeResult(expr))
    }
  }

  def run(context: InterpreterContext, expr: Token, args: List[Token]): (InterpreterContext, ValueResult) = {
    assert(args.length == 1)
    val (argContext, argValue) = Interpreter.run(context, args.head)
    argValue match {
      case ListValueResult(values) => (argContext, BooleanValueResult(values.isEmpty))
      case _ => (argContext, BooleanValueResult(false))
    }
  }
}