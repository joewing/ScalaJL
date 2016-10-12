package net.joewing.jl

case class Scope[T, C <: Context[T, C]](id: Int, parent: Int, values: Map[String, T]) {

  def lookup(context: Context[T, C], name: String): Option[T] = {
    values.get(name) match {
      case result @ Some(_) => result
      case None => context.lookup(parent, name)
    }
  }

  def update(newValues: Map[String, T]): Scope[T, C] = Scope[T, C](id, parent, values ++ newValues)
}
