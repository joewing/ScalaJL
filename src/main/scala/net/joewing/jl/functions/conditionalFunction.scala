package net.joewing.jl.functions

import net.joewing.jl._
import net.joewing.jl.check._
import net.joewing.jl.interpret._

abstract class ConditionalFunction extends SpecialFunction {

  protected def test[T : Ordering](left: T, right: T): Boolean

  import Ordering.Implicits._

  private implicit object ValueResultOrdering extends Ordering[ValueResult] {
    def compare(left: ValueResult, right: ValueResult): Int = {
      (left, right) match {
        case (IntegerValueResult(a), IntegerValueResult(b)) => implicitly[Ordering[Int]].compare(a, b)
        case (StringValueResult(a), StringValueResult(b)) => implicitly[Ordering[String]].compare(a, b)
        case (ListValueResult(a), ListValueResult(b)) => implicitly[Ordering[List[ValueResult]]].compare(a, b)
      }
    }
  }

  def check(context: CheckerContext, args: List[Token]): (CheckerContext, TypeResult) = {
    if (args.length != 2) {
      (context, InvalidTypeResult("wrong number of arguments to conditional"))
    } else {
      val (leftContext, leftType) = Checker.run(context, args.head)
      val (rightContext, rightType) = Checker.run(leftContext, args(1))
      (leftType, rightType) match {
        case (InvalidTypeResult(_), _) => (rightContext, leftType)
        case (_, InvalidTypeResult(_)) => (rightContext, rightType)
        case _ => (rightContext.addEquivalence(leftType, rightType), BooleanTypeResult())
      }
    }
  }

  def run(context: InterpreterContext, args: List[Token]): (InterpreterContext, ValueResult) = {
    assert(args.length == 2)
    val (leftContext, leftValue) = Interpreter.run(context, args.head)
    val (rightContext, rightValue) = Interpreter.run(leftContext, args(1))
    val result = (leftValue, rightValue) match {
      case (BooleanValueResult(a), BooleanValueResult(b)) => test(a, b)
      case (IntegerValueResult(a), IntegerValueResult(b)) => test(a, b)
      case (StringValueResult(a), StringValueResult(b)) => test(a, b)
      case (ListValueResult(a), ListValueResult(b)) => test(a, b)
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
