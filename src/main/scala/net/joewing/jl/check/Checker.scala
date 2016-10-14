package net.joewing.jl.check

import net.joewing.jl._

object Checker extends Runner[TypeResult] {

  override val nil: TypeResult = NilTypeResult()

  override def run(context: Context[TypeResult], token: Token): (Context[TypeResult], TypeResult) = {
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

  private[this] def runExpr(context: Context[TypeResult], tokens: List[Token]): (Context[TypeResult], TypeResult) = {
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
      context: Context[TypeResult],
      name: String,
      args: List[Token]): (Context[TypeResult], TypeResult) = {
    context.lookup(name) match {
      case Some(func) => runFunction(context, func, args)
      case _ => (context, InvalidTypeResult(s"function not found: $name"))
    }
  }

  private[this] def runFunction(
      context: Context[TypeResult],
      func: TypeResult,
      args: List[Token]): (Context[TypeResult], TypeResult) = {
    func match {
      case special @ SpecialTypeResult(_) => runSpecial(context, special, args)
      case _ => (context, InvalidTypeResult("internal"))
    }
  }

  private[this] def runSpecial(
      context: Context[TypeResult],
      special: SpecialTypeResult,
      args: List[Token]): (Context[TypeResult], TypeResult) = {
    special.func.check(context, args)
  }

}
