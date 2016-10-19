package net.joewing.jl.interpret

import net.joewing.jl.{Scope, ScopeId}

class InterpreterScope(
    _id: ScopeId,
    _values: Map[String, ValueResult])
  extends Scope[ValueResult, InterpreterScope](_id, _values) {

  def create(values: Map[String, ValueResult]): InterpreterScope = new InterpreterScope(id, values)

}