package net.joewing.jl.functions

import net.joewing.jl._
import net.joewing.jl.check._
import net.joewing.jl.interpret._

abstract class ConditionalFunction extends SpecialFunction {

  protected def test[T : Ordering](left: T, right: T): Boolean

  def check(context: CheckerContext, args: List[Token]): (CheckerContext, TypeResult) = {
    if (args.length != 2) {
      (context, InvalidTypeResult("wrong number of arguments to conditional"))
    } else {
      val (leftContext, leftValue) = Checker.run(context, args.head)
      val (rightContext, rightValue) = Checker.run(leftContext, args(1))
      (leftValue, rightValue) match {
        case (InvalidTypeResult(_), _) => (rightContext, leftValue)
        case (_, InvalidTypeResult(_)) => (rightContext, rightValue)
        case (IntegerTypeResult(), IntegerTypeResult()) => (rightContext, BooleanTypeResult())
        case (IntegerTypeResult(), UnknownTypeResult(b)) =>
          (rightContext.addBound(b, IntegerTypeResult()), BooleanTypeResult())
        case (UnknownTypeResult(a), IntegerTypeResult()) =>
          (rightContext.addBound(a, IntegerTypeResult()), BooleanTypeResult())
        case (StringTypeResult(), StringTypeResult()) => (rightContext, BooleanTypeResult())
        case (StringTypeResult(), UnknownTypeResult(b)) =>
          (rightContext.addBound(b, StringTypeResult()), BooleanTypeResult())
        case (UnknownTypeResult(a), StringTypeResult()) =>
          (rightContext.addBound(a, StringTypeResult()), BooleanTypeResult())
        case _ => (rightContext, InvalidTypeResult("wrong types to conditional"))
      }
    }
  }

  def run(context: InterpreterContext, args: List[Token]): (InterpreterContext, ValueResult) = {
    assert(args.length == 2)
    val (leftContext, leftValue) = Interpreter.run(context, args.head)
    val (rightContext, rightValue) = Interpreter.run(leftContext, args(1))
    val result = (leftValue, rightValue) match {
      case (IntegerValueResult(a), IntegerValueResult(b)) => test(a, b)
      case (StringValueResult(a), StringValueResult(b)) => test(a, b)
      case _ => false
    }
    (rightContext, BooleanValueResult(result))
  }

}

class EQFunction extends ConditionalFunction {
  def test[T : Ordering](left: T, right: T) = implicitly[Ordering[T]].eq(left, right)
}

class NEFunction extends ConditionalFunction {
  def test[T : Ordering](left: T, right: T) = implicitly[Ordering[T]].ne(left, right)
}

class GTFunction extends ConditionalFunction {
  def test[T : Ordering](left: T, right: T) = implicitly[Ordering[T]].gt(left, right)
}

class GEFunction extends ConditionalFunction {
  def test[T: Ordering](left: T, right: T) = implicitly[Ordering[T]].gteq(left, right)
}

class LTFunction extends ConditionalFunction {
  def test[T : Ordering](left: T, right: T) = implicitly[Ordering[T]].lt(left, right)
}

class LEFunction extends ConditionalFunction {
  def test[T : Ordering](left: T, right: T) = implicitly[Ordering[T]].lteq(left, right)
}
