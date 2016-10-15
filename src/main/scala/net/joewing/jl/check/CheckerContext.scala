package net.joewing.jl.check

import net.joewing.jl.{Context, Scope, ScopeId}

class CheckerContext(
    _stack: List[ScopeId],
    _scopes: Map[ScopeId, Scope[TypeResult]])
  extends Context[TypeResult, CheckerContext](_stack, _scopes) {

  override protected def create(
      stack: List[ScopeId],
      scopes: Map[ScopeId, Scope[TypeResult]]): CheckerContext = {
    new CheckerContext(stack, scopes)
  }

}
