package net.joewing.jl.interpret

import net.joewing.jl._

object Interpreter extends Runner[ValueResult] {

  private[this] def runSpecial(
      context: Context[ValueResult],
      special: SpecialValueResult,
      args: List[Token]): (Context[ValueResult], ValueResult) = {
    special.func.run(context, args)
  }

  private[this] def runLambda(
      context: Context[ValueResult],
      lambda: LambdaValueResult,
      args: List[Token]): (Context[ValueResult], ValueResult) = {
    val actuals = args.map { run(context, _)._2 }
    val newValues = Map(lambda.parameters.zip(actuals): _*)
    val nestedContext = context.pushScope(lambda.scope).updateScope(newValues)
    val (retContext, retValue) = run(nestedContext, lambda.tokens)
    (retContext.popScope, retValue)
  }

  private[this] def runFunction(
      context: Context[ValueResult],
      func: ValueResult,
      args: List[Token]): (Context[ValueResult], ValueResult) = {
    func match {
      case special @ SpecialValueResult(_) => runSpecial(context, special, args)
      case lambda @ LambdaValueResult(_, _, _) => runLambda(context, lambda, args)
      case _ => (context, NilValueResult())
    }
  }

  private[this] def runFunction(
      context: Context[ValueResult],
      name: String,
      args: List[Token]): (Context[ValueResult], ValueResult) = {
    context.lookup(name) match {
      case Some(func) => runFunction(context, func, args)
      case _ => (context, NilValueResult())
    }
  }

  private[this] def runExpr(
      context: Context[ValueResult],
      tokens: List[Token]): (Context[ValueResult], ValueResult) = {
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

  override def run(context: Context[ValueResult], token: Token): (Context[ValueResult], ValueResult) = {
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

}
