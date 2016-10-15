package net.joewing.jl.parse

import scala.util.parsing.combinator._

object StringParser extends RegexParsers {

  override def skipWhitespace = false

  private def string: Parser[String] = """[^\"]*""".r

  private def top: Parser[String] = "\"" ~ string ~ "\"" ^^ {
    case "\"" ~ content ~ "\"" => content
  }

  def parse(s: String): ParseResult[String] = parseAll(top, s)

}
