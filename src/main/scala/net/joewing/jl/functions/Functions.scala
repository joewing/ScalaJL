package net.joewing.jl.functions

import net.joewing.jl.BaseResult

object Functions {
  private def wrap[T : BaseResult](func: SpecialFunction) = implicitly[BaseResult[T]].create(func)
  def apply[T : BaseResult](): Map[String, T] = Map(
    "="       -> wrap(new EQFunction),
    "!="      -> wrap(new NEFunction),
    ">"       -> wrap(new GTFunction),
    ">="      -> wrap(new GEFunction),
    "<="      -> wrap(new LEFunction),
    "<"       -> wrap(new LTFunction),
    "+"       -> wrap(new AddFunction),
    "define"  -> wrap(new DefineFunction),
    "empty?"  -> wrap(new EmptyFunction),
    "head"    -> wrap(new HeadFunction),
    "if"      -> wrap(new IfFunction),
    "lambda"  -> wrap(new LambdaFunction),
    "list"    -> wrap(new ListFunction),
    "print"   -> wrap(new PrintFunction),
    "tail"    -> wrap(new TailFunction)
  )
}
