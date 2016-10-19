package net.joewing.jl.il

import net.joewing.jl.{Scope, ScopeId}

class ILScope(
    _id: ScopeId,
    _values: Map[String, Program]
  ) extends Scope[Program, ILScope](_id, _values) {

  def create(values: Map[String, Program]): ILScope = new ILScope(id, values)

}