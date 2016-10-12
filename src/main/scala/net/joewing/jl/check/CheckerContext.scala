package net.joewing.jl.check

import net.joewing.jl._

class CheckerContext(_scopes: Map[Int, Scope[TypeResult, CheckerContext]], _currentScope: Int)
  extends Context[TypeResult, CheckerContext](_scopes, _currentScope) {

  def create(
      newScopes: Map[Int, Scope[TypeResult, CheckerContext]],
      newId: Int): CheckerContext = {
    new CheckerContext(newScopes, newId)
  }

}