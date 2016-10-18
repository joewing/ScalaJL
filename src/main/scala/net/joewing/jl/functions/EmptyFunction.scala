package net.joewing.jl.functions
import net.joewing.jl.Token
import net.joewing.jl.check._
import net.joewing.jl.interpret._

class EmptyFunction extends SpecialFunction {
  def check(context: CheckerContext, args: List[Token]): (CheckerContext, TypeResult) = {
    if (args.length != 1) {
      (context, InvalidTypeResult("wrong number of arguments to empty?"))
    } else {
      val containedType = UnknownTypeResult(new TypeId())
      val listType = ListTypeResult(containedType)
      val (argContext, argType) = Checker.run(context, args.head)
      (argContext.addEquivalence(listType, argType), BooleanTypeResult())
    }
  }

  def run(context: InterpreterContext, args: List[Token]): (InterpreterContext, ValueResult) = {
    assert(args.length == 1)
    val (argContext, argValue) = Interpreter.run(context, args.head)
    argValue match {
      case ListValueResult(values) => (argContext, BooleanValueResult(values.isEmpty))
      case _ => (argContext, BooleanValueResult(false))
    }
  }
}