package net.joewing.jl.check

import net.joewing.jl.{Scope, ScopeId}

class CheckerScope(
    private val generics: Map[TypeId, TypeId],
    _id: ScopeId,
    _values: Map[String, TypeResult])
  extends Scope[TypeResult, CheckerScope](_id, _values) {

  def create(values: Map[String, TypeResult]): CheckerScope = new CheckerScope(generics, id, values)

}