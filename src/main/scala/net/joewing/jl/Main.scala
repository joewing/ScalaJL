package net.joewing.jl

import net.joewing.jl.interpret.Interpreter
import net.joewing.jl.parse.ExpressionParser

object Main {
    def main(args: Array[String]) {
        if (args.length != 1) {
            println("usage: jli <filename>")
            return
        }
        Interpreter.run(ExpressionParser.parseFile(args(0)).get)
    }
}
