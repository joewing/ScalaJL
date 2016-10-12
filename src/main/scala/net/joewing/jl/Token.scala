package net.joewing.jl


sealed trait Token {
    val values: List[Token]
}
trait LiteralToken[T] {
    self: Token =>
    val values: List[Token] = List(this)
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
