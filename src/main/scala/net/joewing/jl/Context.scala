package net.joewing.jl

abstract class Context[T, C <: Context[T, C]](val scopes: Map[Int, Scope[T, C]], val currentScope: Int) {
  self: C =>

  protected def create(newScopes: Map[Int, Scope[T, C]], newId: Int): C

  protected def nextScopeId: Int = {
    Range(scopes.size, -1, -1).find(i => !scopes.contains(i)).get
  }

  def lookup(id: Int, name: String): Option[T] = {
    if (id >= 0) scopes(id).lookup(this, name) else None
  }

  def lookup(name: String): Option[T] = lookup(currentScope, name)

  def updateScope(newValues: Map[String, T]): C = {
    val scope = scopes(currentScope).update(newValues)
    val updatedScopes = scopes.updated(currentScope, scope)
    create(updatedScopes, currentScope)
  }

  // Enter a new lexical scope.
  def newScope: C = {
    val newScopeId = nextScopeId
    val scope = Scope[T, C](newScopeId, currentScope, Map())
    create(scopes + (newScopeId -> scope), newScopeId)
  }

  // Leave the current lexical scope.
  def pop: C = create(scopes, scopes(currentScope).parent)

  def valueList(names: List[String]): List[T] = names.map { lookup(_).get }

}
