package net.joewing.jl

class Scope[T](val id: Int, val parent: Int, val values: Map[String, T]) {

  def update(newValues: Map[String, T]): Scope[T] = new Scope[T](id, parent, values ++ newValues)

}
