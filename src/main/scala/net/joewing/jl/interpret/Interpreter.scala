package net.joewing.jl.interpret

import net.joewing.jl._
import net.joewing.jl.functions.Functions

object Interpreter extends Runner[ValueResult] {

  private val builtins = Functions() map { case (k, v) => k -> SpecialValueResult(v) }

  private val baseScope = new Scope[ValueResult](0, -1, builtins)

  private val baseContext = new Context[ValueResult](List(), Map(0 -> baseScope), 0)

  private def runSpecial(
      context: Context[ValueResult],
      special: SpecialValueResult,
      args: List[Token]): (Context[ValueResult], ValueResult) = {
    special.func.run(context, args)
  }

  private def runLambda(context: Context[ValueResult], lambda: LambdaValueResult, args: List[Token]): (Context[ValueResult], ValueResult) = {
    val actuals = args.map { run(context, _)._2 }
    val newValues = Map(lambda.parameters.zip(actuals): _*)
    val nestedContext = context.pushScope(lambda.scope).updateScope(newValues)
    val (retContext, retValue) = run(nestedContext, lambda.tokens)
    (retContext.popScope, retValue)
  }

  private def runFunction(context: Context[ValueResult], func: ValueResult, args: List[Token]): (Context[ValueResult], ValueResult) = {
    func match {
      case special @ SpecialValueResult(_) => runSpecial(context, special, args)
      case lambda @ LambdaValueResult(_, _, _) => runLambda(context, lambda, args)
      case _ => (context, NilValueResult())
    }
  }

  private def runFunction(context: Context[ValueResult], name: String, args: List[Token]): (Context[ValueResult], ValueResult) = {
    context.lookup(name) match {
      case Some(func) => runFunction(context, func, args)
      case _ => (context, NilValueResult())
    }
  }

  private def runExpr(context: Context[ValueResult], tokens: List[Token]): (Context[ValueResult], ValueResult) = {
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

  def run(lst: List[Token]): ValueResult = {
    val result = lst.foldLeft((baseContext, NilValueResult()): (Context[ValueResult], ValueResult)) { (acc, token) =>
      val (context, _) = acc
      run(context, token)
    }
    result._2
  }

}
