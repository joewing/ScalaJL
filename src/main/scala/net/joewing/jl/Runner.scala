package net.joewing.jl

import net.joewing.jl.functions.Functions

abstract class Runner[T : BaseResult, C <: Context[T, C]] {

  protected val nil: T

  protected def createContext(stack: List[ScopeId], scopes: Map[ScopeId, Scope[T]]): C

  def run(context: C, token: Token): (C, T)

  private[this] val builtins = Functions.apply[T]()

  private[this] val baseScope: Scope[T] = new Scope[T](new ScopeId(), builtins)

  private[this] val baseContext: C = createContext(List(baseScope.id), Map(baseScope.id -> baseScope))

  final def run(context: C, tokens: List[Token]): (C, T) = {
    tokens.foldLeft((context, nil)) { (acc, token) =>
      val (oldContext, _) = acc
      run(oldContext, token)
    }
  }

  protected def postprocess(context: C, result: T): T = result

  def run(lst: List[Token]): T = {
    val result = lst.foldLeft((baseContext, nil): (C, T)) { (acc, token) =>
      val (context, _) = acc
      run(context, token)
    }
    postprocess(result._1, result._2)
  }
}
