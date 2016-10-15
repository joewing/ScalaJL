package net.joewing.jl.functions

import net.joewing.jl._
import net.joewing.jl.check._
import net.joewing.jl.interpret._

class LambdaFunction extends SpecialFunction {
  // (lambda (args...) ...)

  private def getParameters(tokens: List[Token]): Option[List[String]] =
    tokens.foldLeft(Some(List()): Option[List[String]]) { (acc, token) =>
      (acc, token) match {
        case (Some(lst), IdentToken(ident)) => Some(lst :+ ident)
        case _ => None
      }
    }

  private def getParameters(expr: Token): Option[List[String]] =
    expr match {
      case ExprToken(tokens) => getParameters(tokens)
      case _ => None
    }

  def check(context: CheckerContext, args: List[Token]): (CheckerContext, TypeResult) = {
    if (args.length < 2) {
      (context, InvalidTypeResult("at least 2 arguments required for lambda"))
    } else {
      getParameters(args.head) match {
        case None => (context, InvalidTypeResult("invalid parameter list"))
        case Some(parameters) =>
          val parameterTypes = parameters.foldLeft(Map(): Map[String, TypeResult]) { (acc, name) =>
            acc + (name -> UnknownTypeResult())
          }
          val nestedContext = context.enterScope.updateScope(parameterTypes)
          val (retContext, retType) = Checker.run(nestedContext, args.tail)
          val newType = LambdaTypeResult(nestedContext.stack, retContext.valueList(parameters), retType)
          val newContext = retContext.leaveScope
          (newContext, newType)
      }
    }
  }

  def run(context: InterpreterContext, args: List[Token]): (InterpreterContext, ValueResult) = {
    assert(args.length >= 2)
    getParameters(args.head) match {
      case None => (context, NilValueResult())
      case Some(parameters) =>
        (context, LambdaValueResult(context.stack, parameters, args.tail))
    }
  }

}