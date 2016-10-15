package net.joewing.jl.interpret

import net.joewing.jl._

object Interpreter extends Runner[ValueResult, InterpreterContext] {

  private[this] def runSpecial(
      context: InterpreterContext,
      special: SpecialValueResult,
      args: List[Token]): (InterpreterContext, ValueResult) = {
    special.func.run(context, args)
  }

  private[this] def runLambda(
      context: InterpreterContext,
      lambda: LambdaValueResult,
      args: List[Token]): (InterpreterContext, ValueResult) = {
    val actuals = args.map { run(context, _)._2 }
    val newValues = Map(lambda.parameters.zip(actuals): _*)
    val nestedContext = context.pushScope(lambda.stack).updateScope(newValues)
    val (retContext, retValue) = run(nestedContext, lambda.tokens)
    (retContext.popScope(lambda.stack), retValue)
  }

  private[this] def runFunction(
      context: InterpreterContext,
      func: ValueResult,
      args: List[Token]): (InterpreterContext, ValueResult) = {
    func match {
      case special @ SpecialValueResult(_) => runSpecial(context, special, args)
      case lambda @ LambdaValueResult(_, _, _) => runLambda(context, lambda, args)
      case _ => (context, NilValueResult())
    }
  }

  private[this] def runFunction(
      context: InterpreterContext,
      name: String,
      args: List[Token]): (InterpreterContext, ValueResult) = {
    context.lookup(name) match {
      case Some(func) => runFunction(context, func, args)
      case _ => (context, NilValueResult())
    }
  }

  private[this] def runExpr(
      context: InterpreterContext,
      tokens: List[Token]): (InterpreterContext, ValueResult) = {
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

  def createContext(
      stack: List[ScopeId],
      scopes: Map[ScopeId, Scope[ValueResult]]): InterpreterContext = new InterpreterContext(stack, scopes)

  val nil: ValueResult = NilValueResult()

  def run(context: InterpreterContext, token: Token): (InterpreterContext, ValueResult) = {
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
