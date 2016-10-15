package net.joewing.jl.parse

import java.io.FileReader
import net.joewing.jl._
import scala.util.parsing.combinator._

object ExpressionParser extends JavaTokenParsers {

  private def number = wholeNumber ^^ { s => IntegerToken(s.toInt) }

  private def string = stringLiteral ^^ { s => StringToken(StringParser.parse(s).get) }

  private def identifier: Parser[Token] = ident ^^ {
    case "true" => BooleanToken(true)
    case "false" => BooleanToken(false)
    case s => IdentToken(s)
  }

  private def value: Parser[Token] = number | string | identifier | expr

  private def expr: Parser[Token] = "(" ~ value.* ~ ")" ^^ {
    case "(" ~ lst ~ ")" => ExprToken(lst)
  }

  private def top: Parser[List[Token]] = value.*

  def parse(s: String): ParseResult[List[Token]] = parseAll(top, s)

  def parseFile(filename: String): ParseResult[List[Token]] = {
    val reader = new FileReader(filename)
    parseAll(top, reader)
  }

}
