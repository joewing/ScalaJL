package net.joewing.jl.check

import net.joewing.jl._

object Checker extends Runner[TypeResult, CheckerContext] {

  protected def createContext(
      stack: List[ScopeId],
      scopes: Map[ScopeId, Scope[TypeResult]]): CheckerContext = new CheckerContext(Map(), Map(), stack, scopes)

  val nil: TypeResult = NilTypeResult()

  def run(context: CheckerContext, token: Token): (CheckerContext, TypeResult) = {
    token match {
      case IdentToken(ident) =>
        context.lookup(ident) match {
          case Some(result) => (context, result)
          case None => (context, InvalidTypeResult(s"not found: $ident"))
        }
      case BooleanToken(_) => (context, BooleanTypeResult())
      case IntegerToken(_) => (context, IntegerTypeResult())
      case StringToken(_) => (context, StringTypeResult())
      case ExprToken(tokens) => runExpr(context, tokens)
    }
  }

  override def postprocess(context: CheckerContext, result: TypeResult): TypeResult = context.solve(result)

  private[this] def runExpr(context: CheckerContext, tokens: List[Token]): (CheckerContext, TypeResult) = {
    tokens match {
      case IdentToken(name) :: args => runFunction(context, name, args)
      case ExprToken(lst) :: args =>
        val (newContext, func) = runExpr(context, lst)
        runFunction(newContext, func, args)
      case _ =>
        val msg = s"invalid expression: $tokens"
        println(msg)
        (context, InvalidTypeResult(msg))
    }
  }

  private[this] def runFunction(
      context: CheckerContext,
      name: String,
      args: List[Token]): (CheckerContext, TypeResult) = {
    context.lookup(name) match {
      case Some(func) => runFunction(context, func, args)
      case _ => (context, InvalidTypeResult(s"function not found: $name"))
    }
  }

  private[this] def runFunction(
      context: CheckerContext,
      func: TypeResult,
      args: List[Token]): (CheckerContext, TypeResult) = {
    func match {
      case special @ SpecialTypeResult(_) => runSpecial(context, special, args)
      case _ => (context, InvalidTypeResult("internal"))
    }
  }

  private[this] def runSpecial(
      context: CheckerContext,
      special: SpecialTypeResult,
      args: List[Token]): (CheckerContext, TypeResult) = {
    special.func.check(context, args)
  }

}
