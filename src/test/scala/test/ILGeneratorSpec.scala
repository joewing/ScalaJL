package test

import net.joewing.jl.il.{ILGenerator, IntegerInstruction, ProgramGraph, ProgramResult}
import net.joewing.jl.parse.ExpressionParser
import org.scalatest.{FlatSpec, Matchers}

class ILGeneratorSpec extends FlatSpec with Matchers {

  private[this] def generateProgram(code: String): ProgramResult = {
    val program = new ExpressionParser("test").parse(code).get
    ILGenerator.run(program)
  }

  private[this] def getParts(result: ProgramResult): Seq[String] = {
    result.toString.split('\n').filter(!_.startsWith("L")).map(_.stripPrefix("\t"))
  }

  "integer literals" should "be integers" in {
    val result = generateProgram("1")
    assert(getParts(result) == Seq("int(1)"))
  }

  "add" should "add integers" in {
    val result = generateProgram("(+ 1 2)")
    println(result)
  }

}
