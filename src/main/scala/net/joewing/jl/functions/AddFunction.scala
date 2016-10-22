package net.joewing.jl.functions

import net.joewing.jl._
import net.joewing.jl.check._
import net.joewing.jl.il._
import net.joewing.jl.interpret._

class AddFunction extends SpecialFunction {
  // (+ ...)

  def check(context: CheckerContext, expr: Token, args: List[Token]): (CheckerContext, TypeResult) = {
    val valueType = IntegerTypeResult(expr)
    context.fold(args)(valueType: TypeResult) { (oldContext, oldType, token) =>
      val (newContext, newType) = Checker.run(oldContext, token)
      val boundContext = newContext.addEquivalence(newType, valueType)
      (boundContext, valueType)
    }
  }

  def run(context: InterpreterContext, expr: Token, args: List[Token]): (InterpreterContext, ValueResult) =
    context.fold(args)(IntegerValueResult(0)) { (oldContext, oldValue, token) =>
      val (newContext, newValue) = Interpreter.run(oldContext, token)
      newValue match {
        case IntegerValueResult(i) => (newContext, IntegerValueResult(oldValue.value + i))
        case _ => (newContext, IntegerValueResult(0))
      }
    }

  override def generate(context: ILContext, args: List[Token]): (ILContext, ValueId) = {
    val (actualContext, actuals) = context.map(args) { (oldContext, token) => ILGenerator.run(oldContext, token) }
    actuals.length match {
      case 0 =>
        val (newContext, result) = actualContext.newValue(IntegerValueType())
        (newContext.addInstruction(IntegerInstruction(new InstructionId, result, 0)), result)
      case 1 => (actualContext, actuals.head)
      case _ =>
        val (valueContext, firstValue) = actualContext.newValue(IntegerValueType())
        val zeroContext = valueContext.addInstruction(
          BinaryInstruction(new InstructionId, Add(), firstValue, actuals.head, actuals(1))
        )
        zeroContext.fold(actuals.drop(2))(firstValue) { (context, acc, value) =>
          val (nextContext, nextValue) = context.newValue(IntegerValueType())
          val updatedContext = nextContext.addInstruction(
            BinaryInstruction(new InstructionId, Add(), nextValue, acc, value)
          )
          (updatedContext, nextValue)
        }
    }
  }
}
