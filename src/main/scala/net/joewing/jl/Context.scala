package net.joewing.jl

class Context[T](
    private val stack: List[ScopeId],
    private val scopes: Map[ScopeId, Scope[T]]) {

  val currentScope = stack.head

  private[this] def lookupStack(name: String, lst: List[ScopeId]): Option[T] = lst match {
    case id :: tl => scopes(id).values.get(name).orElse(lookupStack(name, tl))
    case _ =>
      println(s"not found: $name")
      None
  }

  def lookup(name: String): Option[T] = lookupStack(name, stack)

  def updateScope(newValues: Map[String, T]): Context[T] = {
    val scope = scopes(stack.head).update(newValues)
    val updatedScopes = scopes.updated(stack.head, scope)
    new Context[T](stack, updatedScopes)
  }

  def valueList(names: List[String]): List[T] = names.map { lookup(_).get }

  private[this] def getReferences(id: ScopeId): Set[ScopeId] = {
    scopes(id).values.collect { case LambdaResult(i) => getReferences(i) }.flatten.toSet + id
  }

  private[this] def gc: Context[T] = {
    val allScopes = scopes.keySet
    val referencedScopes = stack.foldLeft(Set[ScopeId]()) { _ ++ getReferences(_) }
    val toRemove = allScopes -- referencedScopes
    val newScopes = scopes -- toRemove
    new Context[T](stack, newScopes)
  }

  def enterScope: Context[T] = {
    val newScopeId = new ScopeId()
    val scope = new Scope[T](newScopeId, Map())
    new Context[T](newScopeId +: stack, scopes + (newScopeId -> scope))
  }

  def leaveScope: Context[T] = new Context[T](stack.tail, scopes)

  def pushScope(root: ScopeId): Context[T] = new Context[T](root +: stack, scopes).enterScope

  def popScope: Context[T] = gc.leaveScope.leaveScope

}
