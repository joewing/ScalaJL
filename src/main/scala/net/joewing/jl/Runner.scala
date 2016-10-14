package net.joewing.jl

import net.joewing.jl.functions.Functions

abstract class Runner[T : BaseResult] {

  protected val nil: T

  def run(context: Context[T], token: Token): (Context[T], T)

  private val builtins = Functions.apply[T]()

  private val baseScope = new Scope[T](0, -1, builtins)

  private val baseContext = new Context[T](List(), Map(0 -> baseScope), 0)

  final def run(context: Context[T], tokens: List[Token]): (Context[T], T) = {
    tokens.foldLeft((context, nil)) { (acc, token) =>
      val (oldContext, _) = acc
      run(oldContext, token)
    }
  }

  final def run(lst: List[Token]): T = {
    val result = lst.foldLeft((baseContext, nil): (Context[T], T)) { (acc, token) =>
      val (context, _) = acc
      run(context, token)
    }
    result._2
  }
}
