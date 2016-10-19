package net.joewing.jl.check

import net.joewing.jl.{Scope, ScopeId}

class CheckerScope(
    val generics: Map[TypeId, TypeResult],
    _id: ScopeId,
    _values: Map[String, TypeResult])
  extends Scope[TypeResult, CheckerScope](_id, _values) {

  def create(values: Map[String, TypeResult]): CheckerScope = new CheckerScope(generics, id, values)

  def setGeneric(anyId: TypeId, other: TypeResult): CheckerScope =
    new CheckerScope(generics + (anyId -> other), id, values)

}