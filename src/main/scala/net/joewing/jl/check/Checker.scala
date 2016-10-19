package net.joewing.jl.check

import net.joewing.jl._

object Checker extends Runner[TypeResult, CheckerScope, CheckerContext] {

  protected def createContext(
      stack: List[ScopeId],
      scopes: Map[ScopeId, CheckerScope]): CheckerContext = new CheckerContext(Map(), Map(), stack, scopes)

  val nil: TypeResult = NilTypeResult(InvalidToken())

  def run(context: CheckerContext, token: Token): (CheckerContext, TypeResult) = {
    token match {
      case IdentToken(ident) =>
        context.lookup(ident) match {
          case Some(result) => (context, result)
          case None => (context, InvalidTypeResult(token, s"not found: $ident"))
        }
      case _: BooleanToken => (context, BooleanTypeResult(token))
      case _: IntegerToken => (context, IntegerTypeResult(token))
      case _: StringToken => (context, StringTypeResult(token))
      case _: InvalidToken => (context, InvalidTypeResult(token, s"invalid token"))
      case ExprToken(tokens) => runExpr(context, token, tokens)
    }
  }

  override def postprocess(context: CheckerContext, result: TypeResult): TypeResult = {
    val resultType = context.solve(result)
    if (resultType.isUnknown) {
      InvalidTypeResult(result.token, s"could not resolve type of $result")
    } else {
      resultType
    }
  }

  private[this] def runExpr(
      context: CheckerContext,
      token: Token,
      tokens: List[Token]): (CheckerContext, TypeResult) = {
    tokens match {
      case IdentToken(name) :: args => runFunction(context, token, name, args)
      case (exprToken @ ExprToken(lst)) :: args =>
        val (newContext, func) = runExpr(context, exprToken, lst)
        runFunction(newContext, exprToken, func, args)
      case _ => (context, InvalidTypeResult(token, "invalid expression"))
    }
  }

  private[this] def runFunction(
      context: CheckerContext,
      token: Token,
      name: String,
      args: List[Token]): (CheckerContext, TypeResult) = {
    context.lookup(name) match {
      case Some(func) => runFunction(context, token, func, args)
      case None => (context, InvalidTypeResult(token, s"function not found: $name"))
    }
  }

  private[this] def runFunction(
      context: CheckerContext,
      token: Token,
      func: TypeResult,
      args: List[Token]): (CheckerContext, TypeResult) = {
    func match {
      case special: SpecialTypeResult => runSpecial(context, token, special, args)
      case lambda: LambdaTypeResult => runLambda(context, token, lambda, args)
      case unknown: UnknownTypeResult => runUnknown(context, token, unknown, args)
      case _ => (context, InvalidTypeResult(token, s"not a function: $func"))
    }
  }

  private[this] def getParameterTypes(
      context: CheckerContext,
      args: List[Token]): (CheckerContext, List[TypeResult]) = {
    val zero = (context, List[TypeResult]())
    args.foldLeft(zero) { (acc, token) =>
      val (oldContext, oldList) = acc
      val (newContext, newType) = run(oldContext, token)
      (newContext, oldList :+ newType)
    }
  }

  private[this] def runUnknown(
      context: CheckerContext,
      token: Token,
      unknown: UnknownTypeResult,
      args: List[Token]): (CheckerContext, TypeResult) = {
    val (paramContext, paramTypes) = getParameterTypes(context, args)
    val retType = UnknownTypeResult(token, new TypeId())
    val lambdaType = LambdaTypeResult(token, paramTypes, retType)
    val boundedContext = paramContext.addEquivalence(unknown, lambdaType)
    (boundedContext, retType)
  }

  private[this] def runLambda(
      context: CheckerContext,
      token: Token,
      lambda: LambdaTypeResult,
      args: List[Token]): (CheckerContext, TypeResult) = {
    val (paramContext, paramTypes) = getParameterTypes(context, args)
    val equivContext = lambda.args.zip(paramTypes).foldLeft(paramContext) { (context, types) =>
      val (actual, expected) = types
      context.addEquivalence(actual, expected)
    }
    (equivContext, lambda.ret)
  }

  private[this] def runSpecial(
      context: CheckerContext,
      token: Token,
      special: SpecialTypeResult,
      args: List[Token]): (CheckerContext, TypeResult) = {
    special.func.check(context, token, args)
  }

}
