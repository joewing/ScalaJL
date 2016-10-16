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

  override def postprocess(context: CheckerContext, result: TypeResult): TypeResult = {
    context.solve(result) match {
      case UnknownTypeResult(_) => InvalidTypeResult("could not resolve type")
      case other => other
    }
  }

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
      case lambda @ LambdaTypeResult(_, _, _) => runLambda(context, lambda, args)
      case unknown @ UnknownTypeResult(id) => runUnknown(context, unknown, args)
      case _ => (context, InvalidTypeResult(s"not a function: $func"))
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
      unknown: UnknownTypeResult,
      args: List[Token]): (CheckerContext, TypeResult) = {
    val (paramContext, paramTypes) = getParameterTypes(context, args)
    val retType = UnknownTypeResult(new TypeId())
    val lambdaType = LambdaTypeResult(List(), paramTypes, retType)
    val boundedContext = paramContext.addBound(unknown.id, lambdaType)
    (boundedContext, retType)
  }

  private[this] def runLambda(
      context: CheckerContext,
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
      special: SpecialTypeResult,
      args: List[Token]): (CheckerContext, TypeResult) = {
    special.func.check(context, args)
  }

}
