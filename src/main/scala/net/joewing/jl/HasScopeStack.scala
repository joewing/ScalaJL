package net.joewing.jl

trait HasScopeStack {
  val stack: List[ScopeId]
}

object HasScopeStack {
  def unapply(lambda: HasScopeStack): Option[List[ScopeId]] = Some(lambda.stack)
}
