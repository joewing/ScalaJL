package net.joewing.jl.functions

object Functions {
  def apply(): Map[String, SpecialFunction] = Map(
    "eq"      -> new EQFunction,
    "ne"      -> new NEFunction,
    "gt"      -> new GTFunction,
    "ge"      -> new GEFunction,
    "le"      -> new LEFunction,
    "lt"      -> new LTFunction,
    "add"     -> new AddFunction,
    "define"  -> new DefineFunction,
    "if"      -> new IfFunction,
    "lambda"  -> new LambdaFunction,
    "print"   -> new PrintFunction
  )
}
