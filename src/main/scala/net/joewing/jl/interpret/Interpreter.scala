package net.joewing.jl.interpret

import net.joewing.jl._

object Interpreter extends Runner[ValueResult, InterpreterScope, InterpreterContext] {

  private[this] def runSpecial(
      context: InterpreterContext,
      token: Token,
      special: SpecialValueResult,
      args: List[Token]): (InterpreterContext, ValueResult) = {
    special.func.run(context, token, args)
  }

  private[this] def runLambda(
      context: InterpreterContext,
      token: Token,
      lambda: LambdaValueResult,
      args: List[Token]): (InterpreterContext, ValueResult) = {
    val actuals = args.map { run(context, _)._2 }
    val newValues = Map(lambda.parameters.zip(actuals): _*)
    val nestedContext = context.pushScope(lambda.stack).enterScope.updateScope(newValues)
    val (retContext, retValue) = run(nestedContext, lambda.tokens)
    (retContext.leaveScope.popScope(lambda.stack), retValue)
  }

  private[this] def runFunction(
      context: InterpreterContext,
      token: Token,
      func: ValueResult,
      args: List[Token]): (InterpreterContext, ValueResult) = {
    func match {
      case special @ SpecialValueResult(_) => runSpecial(context, token, special, args)
      case lambda @ LambdaValueResult(_, _, _) => runLambda(context, token, lambda, args)
      case _ => (context, NilValueResult())
    }
  }

  private[this] def runFunction(
      context: InterpreterContext,
      token: Token,
      name: String,
      args: List[Token]): (InterpreterContext, ValueResult) = {
    context.lookup(name) match {
      case Some(func) => runFunction(context, token, func, args)
      case _ => (context, NilValueResult())
    }
  }

  private[this] def runExpr(
      context: InterpreterContext,
      tokens: List[Token]): (InterpreterContext, ValueResult) = {
    tokens match {
      case IdentToken(name) :: args => runFunction(context, tokens.head, name, args)
      case ExprToken(lst) :: args =>
        val (newContext, func) = runExpr(context, lst)
        runFunction(newContext, tokens.head, func, args)
      case _ => (context, NilValueResult())
    }
  }

  def createContext(
      stack: List[ScopeId],
      scopes: Map[ScopeId, InterpreterScope]): InterpreterContext = new InterpreterContext(stack, scopes)

  val nil: ValueResult = NilValueResult()

  def run(context: InterpreterContext, token: Token): (InterpreterContext, ValueResult) = {
    token match {
      case IdentToken(ident) => (context, context.lookup(ident).get)
      case BooleanToken(value) => (context, BooleanValueResult(value))
      case IntegerToken(value) => (context, IntegerValueResult(value))
      case StringToken(value) => (context, StringValueResult(value))
      case ExprToken(tokens) => runExpr(context, tokens)
      case InvalidToken() => (context, NilValueResult())
    }
  }

}
