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
          case None =>
            val (file, line) = (token.file, token.line)
            val msg = s"$file[$line]: not found: $ident"
            println(msg)
            (context, InvalidTypeResult(msg))
        }
      case BooleanToken(_) => (context, BooleanTypeResult())
      case IntegerToken(_) => (context, IntegerTypeResult())
      case StringToken(_) => (context, StringTypeResult())
      case ExprToken(tokens) => runExpr(context, token, tokens)
    }
  }

  override def postprocess(context: CheckerContext, result: TypeResult): TypeResult = {
    context.solve(result) match {
      case UnknownTypeResult(_) => InvalidTypeResult(s"could not resolve type of $result")
      case other => other
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
      case _ =>
        val (file, line) = (token.file, token.line)
        val msg = s"$file[$line]: invalid expression: $tokens"
        println(msg)
        (context, InvalidTypeResult(msg))
    }
  }

  private[this] def runFunction(
      context: CheckerContext,
      token: Token,
      name: String,
      args: List[Token]): (CheckerContext, TypeResult) = {
    context.lookup(name) match {
      case Some(func) => runFunction(context, token, func, args)
      case None =>
        val (file, line) = (token.file, token.line)
        val msg = s"$file[$line]: function not found: $name"
        println(msg)
        (context, InvalidTypeResult(msg))
    }
  }

  private[this] def runFunction(
      context: CheckerContext,
      token: Token,
      func: TypeResult,
      args: List[Token]): (CheckerContext, TypeResult) = {
    func match {
      case special @ SpecialTypeResult(_) => runSpecial(context, token, special, args)
      case lambda @ LambdaTypeResult(_, _) => runLambda(context, token, lambda, args)
      case unknown @ UnknownTypeResult(id) => runUnknown(context, token, unknown, args)
      case _ =>
        val (file, line) = (token.file, token.line)
        val msg = s"$file[$line]: not a function: $func"
        println(msg)
        (context, InvalidTypeResult(msg))
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
    val retType = UnknownTypeResult(new TypeId())
    val lambdaType = LambdaTypeResult(paramTypes, retType)
    val boundedContext = paramContext.addBound(unknown.id, lambdaType)
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
    special.func.check(context, args)
  }

}
