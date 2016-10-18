package net.joewing.jl

import scala.util.parsing.input.Positional

sealed trait Token extends Positional {
  var file: String = ""
  def line: Int = pos.line
  val values: List[Token]

  def setFile(name: String): Token = {
    file = name
    this
  }
}
trait LiteralToken[T] {
  self: Token =>
  val values: List[Token] = List(this)
}
case class InvalidToken() extends Token {
  val values = List[Token]()
  override def toString = "<invalid>"
}
case class BooleanToken(value: Boolean) extends Token with LiteralToken[Boolean] {
  override def toString = if (value) "true" else "false"
}
case class IntegerToken(value: Int) extends Token with LiteralToken[Int] {
  override def toString = value.toString
}
case class StringToken(value: String) extends Token with LiteralToken[String] {
  override def toString = value
}
case class IdentToken(value: String) extends Token with LiteralToken[String] {
  override def toString = value
}
case class ExprToken(values: List[Token]) extends Token {
  override def toString = values.mkString("(", " ", ")")
}
