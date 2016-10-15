package net.joewing.jl

class ScopeId()

class Scope[T](val id: ScopeId, val values: Map[String, T]) {

  def update(newValues: Map[String, T]): Scope[T] = new Scope[T](id, values ++ newValues)

}
