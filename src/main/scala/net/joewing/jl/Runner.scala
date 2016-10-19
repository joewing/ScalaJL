package net.joewing.jl

import net.joewing.jl.functions.Functions

abstract class Runner[T : BaseResult, SCOPE <: Scope[T, SCOPE], CONTEXT <: Context[T, SCOPE, CONTEXT]] {

  protected val nil: T

  protected def createContext(stack: List[ScopeId], scopes: Map[ScopeId, SCOPE]): CONTEXT

  def run(context: CONTEXT, token: Token): (CONTEXT, T)

  private[this] val builtins = Functions.apply[T]()

  private[this] val baseContext: CONTEXT = {
    val emptyContext = createContext(List(), Map())
    val baseScope = emptyContext.newScope.update(builtins)
    createContext(List(baseScope.id), Map(baseScope.id -> baseScope))
  }

  final def run(context: CONTEXT, tokens: List[Token]): (CONTEXT, T) = {
    tokens.foldLeft((context, nil)) { (acc, token) =>
      val (oldContext, _) = acc
      run(oldContext, token)
    }
  }

  protected def postprocess(context: CONTEXT, result: T): T = result

  def run(lst: List[Token]): T = {
    val result = lst.foldLeft((baseContext, nil): (CONTEXT, T)) { (acc, token) =>
      val (context, _) = acc
      run(context, token)
    }
    postprocess(result._1, result._2)
  }
}
