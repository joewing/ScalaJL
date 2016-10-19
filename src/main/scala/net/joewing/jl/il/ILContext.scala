package net.joewing.jl.il

import net.joewing.jl._

class ILContext(
    _stack: List[ScopeId],
    _scopes: Map[ScopeId, ILScope]
  ) extends Context[Program, ILScope, ILContext](_stack, _scopes) {

  protected def create(stack: List[ScopeId], scopes: Map[ScopeId, ILScope]): ILContext = ???

  def newScope: ILScope = new ILScope(new ScopeId(), Map())
}