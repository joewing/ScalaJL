package net.joewing.jl.il

import net.joewing.jl._

class ILGenerator {

  private[this] def generateFunction(context: ILContext, func: Program, args: List[Token]): (ILContext, Program) = {
    val zero = (context, List[ProgramId]())
    val (newContext, actuals) = args.foldLeft(zero) { (acc, token) =>
      val (inputContext, inputActuals) = acc
      val (updatedContext, actual) = generate(inputContext, token)
      val updatedActuals = inputActuals :+ actual.id
      (updatedContext, updatedActuals)
    }
    (newContext, CallInstruction(func.id, actuals).program)
  }

  private[this] def generateExpr(context: ILContext, tokens: List[Token]): (ILContext, Program) = tokens match {
    case IdentToken(name) :: args =>
      val func = context.lookup(name).get
      generateFunction(context, func, args)
    case ExprToken(lst) :: args =>
      val (newContext, func) = generateExpr(context, lst)
      generateFunction(newContext, func, args)
  }

  def generate(context: ILContext, token: Token): (ILContext, Program) = token match {
    case IdentToken(name) => (context, context.lookup(name).get)
    case BooleanToken(value) => (context, IntegerInstruction(if (value) 1 else 0).program)
    case IntegerToken(value) => (context, IntegerInstruction(value).program)
    case StringToken(value) => (context, StringInstruction(value).program)
    case ExprToken(values) => generateExpr(context, values)
  }

}