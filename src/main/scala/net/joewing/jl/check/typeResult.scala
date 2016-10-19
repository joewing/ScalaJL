package net.joewing.jl.check

import net.joewing.jl._
import net.joewing.jl.functions.SpecialFunction

class TypeId {
  override def toString = hashCode.toString
}

trait TypeResult {
  val token: Token
  def solve(context: CheckerContext): TypeResult = this
  def isUnknown: Boolean = false
}

case class InvalidTypeResult(token: Token, msgs: List[String]) extends TypeResult {
  override def toString: String = msgs.sorted.mkString("\n")
}

case class UnknownTypeResult(token: Token, id: TypeId) extends TypeResult {
  override def solve(context: CheckerContext): TypeResult = context.solve(token, id)
  override def isUnknown = true
  override def toString = s"<unknown $id>"
}

case class AnyTypeResult(token: Token, id: TypeId) extends TypeResult {
  override def solve(context: CheckerContext): TypeResult = context.solve(token, id)
  override def toString = s"<any $id>"
  override def equals(obj: Any): Boolean = obj match {
    case a: AnyTypeResult => a.id == id
    case _ => false
  }
}

case class NilTypeResult(token: Token) extends TypeResult {
  override def toString = "nil"
  override def equals(obj: Any): Boolean = obj.isInstanceOf[NilTypeResult]
  override def hashCode: Int = 1
}

case class BooleanTypeResult(token: Token) extends TypeResult {
  override def toString = "boolean"
  override def equals(obj: Any): Boolean = obj.isInstanceOf[BooleanTypeResult]
  override def hashCode: Int = 2
}

case class IntegerTypeResult(token: Token) extends TypeResult {
  override def toString = "integer"
  override def equals(obj: Any): Boolean = obj.isInstanceOf[IntegerTypeResult]
  override def hashCode: Int = 3
}

case class StringTypeResult(token: Token) extends TypeResult {
  override def toString = "string"
  override def equals(obj: Any): Boolean = obj.isInstanceOf[StringTypeResult]
  override def hashCode: Int = 4
}

case class SpecialTypeResult(token: Token, func: SpecialFunction) extends TypeResult {
  override def toString = s"<special $func>"
  override def equals(obj: Any): Boolean = obj match {
    case s: SpecialTypeResult => s.func == func
    case _: AnyTypeResult => true
    case _ => false
  }
  override def hashCode: Int = func.hashCode
}

case class LambdaTypeResult(token: Token, args: List[TypeResult], ret: TypeResult) extends TypeResult {

  override def solve(context: CheckerContext): TypeResult = {
    LambdaTypeResult(token, args.map(_.solve(context)), ret.solve(context))
  }

  override def isUnknown = ret.isUnknown || args.exists(_.isUnknown)

  private[this] def fix(t: TypeResult): TypeResult = t match {
    case UnknownTypeResult(unknownToken, id) => AnyTypeResult(unknownToken, id)
    case _ => t
  }

  def fix(context: CheckerContext): LambdaTypeResult = {
    val newArgs = args.map(arg => fix(arg.solve(context)))
    val newRet = fix(ret.solve(context))
    LambdaTypeResult(token, newArgs, newRet)
  }

  private[this] def argString = args.mkString(" -> ")

  override def toString = s"(lambda $argString => $ret)"

  override def equals(obj: Any): Boolean = obj match {
    case l: LambdaTypeResult => l.args == args && l.ret == ret
    case _ => false
  }

  override def hashCode: Int = args.hashCode + ret.hashCode
}

case class ListTypeResult(token: Token, contained: TypeResult) extends TypeResult {

  override def solve(context: CheckerContext): TypeResult = {
    ListTypeResult(token, contained.solve(context))
  }

  override def isUnknown = contained.isUnknown

  override def toString = s"list[$contained]"

  override def equals(obj: Any): Boolean = obj match {
    case lst: ListTypeResult => lst.contained == contained
    case _ => false
  }

}

object InvalidTypeResult {

  def apply(token: Token, msg: String): InvalidTypeResult = {
    val newMessage = s"${token.file}[${token.line}]: $msg"
    InvalidTypeResult(token, List(newMessage))
  }

  def apply(left: InvalidTypeResult, right: InvalidTypeResult): InvalidTypeResult = {
    InvalidTypeResult(left.token, left.msgs ++ right.msgs)
  }

}
