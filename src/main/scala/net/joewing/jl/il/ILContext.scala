package net.joewing.jl.il

import net.joewing.jl._

class ILContext(
    _stack: List[ScopeId],
    _scopes: Map[ScopeId, Scope[Program]])
  extends Context[Program, ILContext](_stack, _scopes) {

  override protected def create(stack: List[ScopeId], scopes: Map[ScopeId, Scope[Program]]): ILContext = ???

}