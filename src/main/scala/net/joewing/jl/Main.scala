package net.joewing.jl

import net.joewing.jl.check.{Checker, InvalidTypeResult}
import net.joewing.jl.interpret.Interpreter
import net.joewing.jl.parse.ExpressionParser

object Main {
    def main(args: Array[String]) {
        if (args.length != 1) {
            println("usage: jli <filename>")
            return
        }
        val program = new ExpressionParser(args(0)).parseFile.get
        Checker.run(program) match {
          case InvalidTypeResult(msg) => println(s"TYPE ERROR: $msg")
          case _ => Interpreter.run(program)
        }
    }
}
