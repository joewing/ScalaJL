package net.joewing.jl

class Context[T](
    private val stack: List[Int],
    private val scopes: Map[Int, Scope[T]],
    val currentScope: Int) {

  protected def nextScopeId: Int = {
    Range(scopes.size, -1, -1).find(i => !scopes.contains(i)).get
  }

  private def lookupStack(id: Int, name: String): Option[T] = scopes.get(id) match {
    case Some(scope) => scope.values.get(name).orElse(lookupStack(scope.parent, name))
    case _ => None
  }

  def lookup(name: String): Option[T] = lookupStack(currentScope, name)

  def updateScope(newValues: Map[String, T]): Context[T] = {
    val scope = scopes(currentScope).update(newValues)
    val updatedScopes = scopes.updated(currentScope, scope)
    new Context[T](stack, updatedScopes, currentScope)
  }

  def valueList(names: List[String]): List[T] = names.map { lookup(_).get }

  private def getReferences(scope: Scope[T]): Set[Int] = {
    scope.values.collect { case LambdaResult(id) => id }.toSet
  }

  private def gc: Context[T] = {
    val allScopes = scopes.keySet
    val topScopes = Set[Int](currentScope) ++ stack
    val referencedScopes = topScopes.foldLeft(topScopes) { (scopeSet, scopeId) =>
      scopeSet ++ getReferences(scopes(scopeId))
    }
    val toRemove = allScopes -- referencedScopes
    val newScopes = scopes -- toRemove
    new Context[T](stack, newScopes, currentScope)
  }

  def enterScope: Context[T] = {
    val newScopeId = nextScopeId
    val scope = new Scope[T](newScopeId, currentScope, Map())
    new Context[T](stack, scopes + (newScopeId -> scope), newScopeId)
  }

  def leaveScope: Context[T] = new Context[T](stack, scopes, scopes(currentScope).parent)

  def pushScope(root: Int): Context[T] = new Context[T](currentScope +: stack, scopes, root).enterScope

  def popScope: Context[T] = new Context[T](stack.tail, scopes, stack.head).gc

}
