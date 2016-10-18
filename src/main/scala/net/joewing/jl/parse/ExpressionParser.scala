package net.joewing.jl.parse

import java.io.FileReader
import net.joewing.jl._
import scala.util.parsing.combinator._

class ExpressionParser(val filename: String) extends JavaTokenParsers {

  private def number = wholeNumber ^^ { s => IntegerToken(s.toInt).setFile(filename) }

  private def string = stringLiteral ^^ { s => StringToken(StringParser.parse(s).get).setFile(filename) }

  private def identifier: Parser[Token] = """[^\(\)" ]+""".r ^^ {
    case "true" => BooleanToken(true).setFile(filename)
    case "false" => BooleanToken(false).setFile(filename)
    case s => IdentToken(s).setFile(filename)
  }

  private def value: Parser[Token] = positioned(number | string | identifier | expr)

  private def expr: Parser[Token] = positioned("(" ~ value.* ~ ")" ^^ {
    case "(" ~ lst ~ ")" => ExprToken(lst).setFile(filename)
  })

  private def top: Parser[List[Token]] = value.*

  def parse(s: String): ParseResult[List[Token]] = parseAll(top, s)

  def parseFile: ParseResult[List[Token]] = parseAll(top, new FileReader(filename))

}
