package net.joewing.jl

class ScopeId {
  override def toString = hashCode.toString
}

abstract class Scope[T, SCOPE <: Scope[T, SCOPE]](val id: ScopeId, val values: Map[String, T]) {

  def create(values: Map[String, T]): SCOPE

  def update(newValues: Map[String, T]): SCOPE = create(values ++ newValues)

}
