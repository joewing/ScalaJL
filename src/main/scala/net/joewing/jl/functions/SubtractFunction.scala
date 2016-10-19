package net.joewing.jl.functions

import net.joewing.jl.Token
import net.joewing.jl.check._
import net.joewing.jl.interpret.{IntegerValueResult, Interpreter, InterpreterContext, ValueResult}

class SubtractFunction extends SpecialFunction {
  // (- ...)

  def check(context: CheckerContext, expr: Token, args: List[Token]): (CheckerContext, TypeResult) = {
    if (args.length < 1) {
      (context, InvalidTypeResult(expr, "too few arguments to -"))
    } else {
      val valueType = IntegerTypeResult(expr)
      context.fold(args)(valueType: TypeResult) { (oldContext, oldType, token) =>
        val (newContext, newType) = Checker.run(oldContext, token)
        val boundContext = newContext.addEquivalence(newType, valueType)
        (boundContext, valueType)
      }
    }
  }

  def run(context: InterpreterContext, expr: Token, args: List[Token]): (InterpreterContext, ValueResult) = {
    assert(args.length > 0)
    val (initContext, initValue) = Interpreter.run(context, args.head)
    assert(initValue.isInstanceOf[IntegerValueResult])
    initContext.fold(args.tail)(initValue.asInstanceOf[IntegerValueResult]) { (oldContext, oldValue, token) =>
      val (newContext, newValue) = Interpreter.run(oldContext, token)
      newValue match {
        case IntegerValueResult(i) => (newContext, IntegerValueResult(oldValue.value - i))
        case _ => (newContext, IntegerValueResult(0))
      }
    }
  }
}