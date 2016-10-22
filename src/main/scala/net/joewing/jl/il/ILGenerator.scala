package net.joewing.jl.il

import net.joewing.jl._

object ILGenerator extends Runner[ValueId, ILScope, ILContext] {

  private[this] def runLambda(context: ILContext, func: Program, args: List[Token]): (ILContext, ValueId) = {
    val zero = (context, List[ValueId]())
    val (newContext, actuals) = args.foldLeft(zero) { (acc, token) =>
      val (inputContext, inputActuals) = acc
      val (updatedContext, actual) = run(inputContext, token)
      val updatedActuals = inputActuals :+ actual.id
      (updatedContext, updatedActuals)
    }
    val dest = new ValueId
    val instruction = CallInstruction(new InstructionId, dest, actuals)
    (newContext.addInstruction(instruction), dest)
  }

  private[this] def runSpecial(context: ILContext, special: SpecialProgram, args: List[Token]): (ILContext, ValueId) = {
    special.func.generate(context, args)
  }

  private[this] def runFunction(context: ILContext, func: ValueId, args: List[Token]): (ILContext, ValueId) = {
    func match {
      case special: SpecialProgram => runSpecial(context, special, args)
      case _ => runLambda(context, func, args)
    }
  }

  private[this] def runExpr(context: ILContext, tokens: List[Token]): (ILContext, ValueId) = tokens match {
    case IdentToken(name) :: args =>
      val func = context.lookup(name).get
      runFunction(context, func, args)
    case ExprToken(lst) :: args =>
      val (newContext, func) = runExpr(context, lst)
      runFunction(newContext, func, args)
  }

  val nil: ValueId = new ValueId

  def createContext(stack: List[ScopeId], scopes: Map[ScopeId, ILScope]): ILContext = {
    new ILContext(Program(new InstructionId, List(), Map(), List()), stack, scopes)
  }

  override protected def postprocess(context: ILContext, result: ValueId): Program = {
    context.program
  }

  def run(context: ILContext, token: Token): (ILContext, ValueId) = token match {
    case IdentToken(name) => (context, context.lookup(name).get)
    case BooleanToken(value) =>
      val (newContext, result) = context.newValue(IntegerValueType())
      (newContext.addInstruction(IntegerInstruction(result, if (value) 1 else 0)), result)
    case IntegerToken(value) =>
      val (newContext, result) = context.newValue(IntegerValueType())
      (newContext.addInstruction(IntegerInstruction(result, value)), result)
    case StringToken(value) =>
      val (newContext, result) = context.newValue(StringValueType())
      (newContext.addInstruction(StringInstruction(result, value)), result)
    case ExprToken(values) => runExpr(context, values)
  }

}