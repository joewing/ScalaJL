package net.joewing.jl.functions

import net.joewing.jl.BaseResult

object Functions {
  private def wrap[T : BaseResult](func: SpecialFunction) = implicitly[BaseResult[T]].create(func)
  def apply[T : BaseResult](): Map[String, T] = Map(
      "eq"      -> wrap(new EQFunction),
      "ne"      -> wrap(new NEFunction),
      "gt"      -> wrap(new GTFunction),
      "ge"      -> wrap(new GEFunction),
      "le"      -> wrap(new LEFunction),
      "lt"      -> wrap(new LTFunction),
      "add"     -> wrap(new AddFunction),
      "define"  -> wrap(new DefineFunction),
      "if"      -> wrap(new IfFunction),
      "lambda"  -> wrap(new LambdaFunction),
      "list"    -> wrap(new ListFunction),
      "print"   -> wrap(new PrintFunction)
    )
}
