package net.joewing.jl.interpret

import net.joewing.jl._
import net.joewing.jl.functions.Functions

object Interpreter extends Runner[ValueResult, InterpreterContext] {

  private val builtins = Functions() map { case (k, v) => k -> SpecialValueResult(v) }

  private val baseScope = Scope[ValueResult, InterpreterContext](0, -1, builtins)

  private val baseContext = new InterpreterContext(List(), Map(0 -> baseScope), 0)

  private def runSpecial(
      context: InterpreterContext,
      special: SpecialValueResult,
      args: List[Token]): (InterpreterContext, ValueResult) = {
    special.func.run(context, args)
  }

  private def runLambda(context: InterpreterContext, lambda: LambdaValueResult, args: List[Token]): (InterpreterContext, ValueResult) = {
    val actuals = args.map { run(context, _)._2 }
    val newValues = Map(lambda.parameters.zip(actuals): _*)
    val nestedContext = context.enterScope(lambda.scope).updateScope(newValues)
    val (retContext, retValue) = run(nestedContext, lambda.tokens)
    (retContext.leaveScope, retValue)
  }

  private def runFunction(context: InterpreterContext, func: ValueResult, args: List[Token]): (InterpreterContext, ValueResult) = {
    func match {
      case special @ SpecialValueResult(_) => runSpecial(context, special, args)
      case lambda @ LambdaValueResult(_, _, _) => runLambda(context, lambda, args)
      case _ => (context, NilValueResult())
    }
  }

  private def runFunction(context: InterpreterContext, name: String, args: List[Token]): (InterpreterContext, ValueResult) = {
    context.lookup(name) match {
      case Some(func) => runFunction(context, func, args)
      case _ => (context, NilValueResult())
    }
  }

  private def runExpr(context: InterpreterContext, tokens: List[Token]): (InterpreterContext, ValueResult) = {
    tokens match {
      case IdentToken(name) :: args => runFunction(context, name, args)
      case ExprToken(lst) :: args =>
        val (newContext, func) = runExpr(context, lst)
        runFunction(newContext, func, args)
      case _ =>
        println(s"invalid expression in runExpr: $tokens")
        (context, NilValueResult())
    }
  }

  override val nil: ValueResult = NilValueResult()

  override def run(context: InterpreterContext, token: Token): (InterpreterContext, ValueResult) = {
    token match {
      case IdentToken(ident) =>
        context.lookup(ident) match {
          case Some(value) => (context, value)
          case None => (context, NilValueResult())
        }
      case BooleanToken(value) => (context, BooleanValueResult(value))
      case IntegerToken(value) => (context, IntegerValueResult(value))
      case StringToken(value) => (context, StringValueResult(value))
      case ExprToken(tokens) => runExpr(context, tokens)
    }
  }

  def run(lst: List[Token]): ValueResult = {
    val result = lst.foldLeft((baseContext, NilValueResult()): (InterpreterContext, ValueResult)) { (acc, token) =>
      val (context, _) = acc
      run(context, token)
    }
    result._2
  }

}
